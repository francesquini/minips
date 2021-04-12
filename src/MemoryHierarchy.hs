module MemoryHierarchy
  (   MemoryHierarchy(..)
    , lineAttributes
    , randomReplacementPolicy
    , lruReplacementPolicy
    , MemoryLevel
    , mkRAM
    , mkCache
    , flushMemoryHierarchy
    , MemoryHierarchyST
    , runMemoryHierarchyST
    , execMemoryHierarchyST
    , AccessType(..)
    , MemoryAccessResponse
    , Log
    , read
    , write
    , AccessStats
    , resetMHStats
    , Latency
  )
where

import Prelude hiding (read)

import Architecture (Address)
import MemoryElement hiding  (tracingEnabled)
import Utils

import Data.Bits
import Data.Functor
import Data.Word
import qualified Data.Vector as V hiding ((//),(!))

import Control.Exception
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

data AccessType = Instruction | Data

data MemoryHierarchy =
    Unified {
        mem :: MemoryLevel
      , lineAttrs :: (Int, Int) -- Shift, Mask
      , tracingEnabled :: Bool
      }
  | Split {
        dataMem :: MemoryLevel
      , instructionMem :: MemoryLevel
      , lineAttrs :: (Int, Int) -- Shift, Mask
      , tracingEnabled :: Bool
      }

type MemoryHierarchyST = StateT MemoryHierarchy (Writer Log)

runMemoryHierarchyST :: MemoryHierarchyST a -> MemoryHierarchy -> MemoryAccessResponse (a, MemoryHierarchy)
runMemoryHierarchyST f s =  runWriter $ runStateT f s

execMemoryHierarchyST :: MemoryHierarchyST a -> MemoryHierarchy -> MemoryHierarchy
execMemoryHierarchyST f = snd . fst . runMemoryHierarchyST f

onMemoryLevelRun :: MemoryLevelST b -> MemoryHierarchyUpdateFun -> MemoryLevel -> MemoryHierarchyST b
onMemoryLevelRun runner updater s = do
  let ((ret, s'), log0) = runMemoryLevelST runner s
  traceST $ fmap ("\t"   <>) log0
  modify $ updater s'
  return ret

fixMem :: MemoryLevel -> MemoryHierarchy -> MemoryHierarchy
fixMem v s = s{mem = v}
fixIC :: MemoryLevel -> MemoryHierarchy -> MemoryHierarchy
fixIC v s = s{instructionMem = v, dataMem = (dataMem s){cNextLevel = cNextLevel v}}
fixDC :: MemoryLevel -> MemoryHierarchy -> MemoryHierarchy
fixDC v s = s{dataMem = v, instructionMem = (instructionMem s){cNextLevel = cNextLevel v}}

updateMemoryHierarchy :: MemoryLevelST a -> MemoryHierarchyST [a]
updateMemoryHierarchy f = do
  mh <- get
  case mh of
       (Unified m _ _)   -> onLvl fixMem m <&> (:[])
       (Split dc ic _ _) -> do
         v0 <- onLvl fixIC ic
         v1 <- onLvl fixDC dc
         return [v0, v1]
  where
    onLvl = onMemoryLevelRun f

flushMemoryHierarchy :: MemoryHierarchyST Latency
flushMemoryHierarchy = sum <$> updateMemoryHierarchy memoryLevelRecursiveFlush

resetMHStats :: MemoryHierarchyST ()
resetMHStats = head <$> updateMemoryHierarchy resetMLStats

access :: AccessType -> MemoryLevelST b -> MemoryHierarchyST b
access at f = do
  mh <- get
  case mh of
    (Unified m _ _)   -> runF fixMem m
    (Split dc ic _ _) ->
      case at of
        Data        -> runF fixDC dc
        Instruction -> runF fixIC ic
  where
    runF = onMemoryLevelRun f

lineAttributes :: Int -> (Int, Int)
lineAttributes lineSz = (countTrailingZeros lineSz, lineSz - 1)

lineShift :: MemoryHierarchy -> Int
lineShift = fst . lineAttrs

lineMask :: MemoryHierarchy -> Int
lineMask = snd . lineAttrs

lineAndIx :: Address -> MemoryHierarchyST (LineIx, Int)
lineAndIx ad = gets lineAndIx'
  where
    lineAndIx' x = (fromIntegral ad `shiftR` lineShift x,
                   (fromIntegral ad .&. lineMask x) `shiftR` 2) -- >>2 since each word has 4 bytes

data MemoryOp = Read | Write

traceST :: Log -> MemoryHierarchyST ()
traceST strs = whenM (gets tracingEnabled) $ tell strs

traceAccess' :: String -> Address -> MemoryHierarchyST ()
traceAccess' prefix addr = do
  mh <- get
  traceST [
    prefix <> " 0x" <> showHex32 addr <>
    " (line# 0x" <> showHex32 (addr `shiftR` lineShift mh) <> ")"]

traceAccess :: AccessType -> MemoryOp -> Address -> MemoryHierarchyST ()
traceAccess Data        Read  = traceAccess' "R"
traceAccess Data        Write = traceAccess' "W"
traceAccess Instruction Read  = traceAccess' "I"
traceAccess Instruction Write = undefined

read :: AccessType -> Address -> MemoryHierarchyST (Word32, Latency)
read aType ad =  assert (ad .&. 0x3 == 0) $ do
  traceAccess aType Read ad
  (l, i) <- lineAndIx ad
  (cl, lat) <- readLine aType l
  return (cl `V.unsafeIndex` i, lat)

-- Escritas são sempre em dados
write :: Address -> Word32 -> MemoryHierarchyST Latency
write ad w32 = assert (ad .&. 0x3 == 0) $ do
  traceAccess Data Write ad
  (l, i) <- lineAndIx ad
  (cl, lat0) <- readLine Data l
  let newLine =  cl `V.unsafeUpd` [(i,w32)]
  lat1 <- writeLine l newLine
  return $ lat1 + lat0

type MemoryHierarchyUpdateFun = MemoryLevel -> MemoryHierarchy -> MemoryHierarchy

readLine :: AccessType -> LineIx -> MemoryHierarchyST (MemoryLine, Latency)
readLine at lnIx = access at (readLineLevel lnIx)

writeLine :: LineIx -> MemoryLine -> MemoryHierarchyST Latency
writeLine lnIx val = access Data (writeLineLevel lnIx val)
