module MinipsST (
      module MinipsST
    , AccessType(..)
    , MemoryAccessResponse
    , MemoryHierarchy
    , Log
    , Latency
    , M.ICount
    , M.rTypeICount, M.iTypeICount, M.jTypeICount
    , M.traceFileHandle
    ) where

import Architecture
import Constants
import MemoryHierarchy
import qualified Minips as M
import Utils

import Control.Monad.State.Strict

import Data.Char
import Data.Binary.IEEE754
import Data.Bits
import Data.Bifunctor
import Data.Function ((&))
import Data.Maybe
import Data.Word as W
import qualified Data.IntMap as IM

import Text.Printf
import System.IO

type MinipsST = StateT M.Minips IO

------------
------------
-- Minips --
------------
------------

makeMinips :: Executable -> MemoryHierarchy -> Either String M.Minips
makeMinips (txt, ro, dt) memHier
  | length txt > maxTextSize @W32 = Left $
      printf ".text section overlaps with .rodata. Maximum .text size %d MB." $
             maxTextSize @Mebi
  | length ro > maxRodataSize @W32  = Left $
      printf ".rodata section overlaps with .data. Maximum .rodata size %.2f MB." $
             maxRodataSize @Mebi
  | otherwise = Right $ M.Minips flushedMemHier regs fpregs (0, 0, 0, 0) 0 Nothing Nothing
  where
    mem0     = zip [textAddress,   textAddress   + 4 ..] txt ++
               zip [roDataAddress, roDataAddress + 4 ..] ro  ++
               zip [dataAddress,   dataAddress   + 4 ..] dt
    fillMemory = do
      mapM_ (uncurry write) mem0
      _ <- flushMemoryHierarchy
      resetMHStats
    flushedMemHier = execMemoryHierarchyST fillMemory memHier
    updateReg r v = IM.update (const $ Just v) (fromEnum r)
    regs =
      IM.fromList (zip (fromEnum <$> [Zero ..]) (repeat 0))
      & updateReg Sp spAddress
      & updateReg Gp gpAddress
      & updateReg Pc pcAddress
    fpregs = IM.fromList (zip (fromEnum <$> [F0 ..]) (repeat 0))

tick :: MinipsST ()
tick = modify M.tick

getStats :: MinipsST (Int, M.ICount, [(String, AccessStats)])
getStats = do
  cycles <- gets M.cycles
  counts <- gets M.iCount
  memStats <- getMemStats
  return (cycles, counts, memStats)

getBranchDelaySlotAddress :: MinipsST (Maybe Word32)
getBranchDelaySlotAddress = gets M.delaySlotAddr

setBranchDelaySlotAddress :: Maybe Word32 -> MinipsST ()
setBranchDelaySlotAddress = modify . M.setBranchDelaySlotAddress

resetBranchDelaySlotAddress :: MinipsST ()
resetBranchDelaySlotAddress = setBranchDelaySlotAddress Nothing

decodeInstruction :: Word32 -> MinipsST InstrWord
decodeInstruction = state . M.decodeInstruction

incPC :: MinipsST ()
incPC = modify M.incPC

incRICount, incIICount, incJICount, incFRCount :: MinipsST ()
incRICount = modify M.incRICount
incIICount = modify M.incIICount
incJICount = modify M.incJICount
incFRCount = modify M.incFRCount

----------
----------
-- GPRs --
----------
----------

regRead :: RegName -> MinipsST Word32
regRead = gets . M.regRead

regWrite :: Integral32 a => RegName -> a -> MinipsST ()
regWrite r = modify . M.regWrite r

infixr 4 !<
(!<) :: Integral32 a => RegName -> a -> MinipsST ()
(!<) = regWrite

----------
----------
-- FPRs --
----------
----------

fpRegRead :: FPRegName -> MinipsST Word32
fpRegRead = gets . M.fpRegRead

fpRegReadFloat :: FPRegName -> MinipsST Float
fpRegReadFloat reg = wordToFloat <$> fpRegRead reg

fpRegReadDouble :: FPRegName -> MinipsST Double
fpRegReadDouble reg = do
  low <- fpRegRead reg
  up <- fpRegRead (succ reg)
  return $ doubleFromWords32 up low

fpRegWrite :: Integral32 a => FPRegName -> a -> MinipsST ()
fpRegWrite r = modify . M.fpRegWrite r

infixr 4 !!<
(!!<) :: Integral32 a => FPRegName -> a -> MinipsST ()
(!!<) = fpRegWrite

fpRegWriteFloat :: FPRegName -> Float -> MinipsST ()
fpRegWriteFloat reg fl = reg !!< floatToWord fl

fpRegWriteDouble :: FPRegName -> Double -> MinipsST ()
fpRegWriteDouble reg db = do
  reg !!< lower
  succ reg !!< upper
  where
    (upper, lower) = words32FromDouble db

------------
------------
-- Memory --
------------
------------

loggingDoAccess :: MemoryAccessResponse a -> MinipsST a
loggingDoAccess (v,lg) = do
  unless (null lg) $ do
    whenM (gets $ isNothing . M.traceFileHandle) $ do
      newHandle <- liftIO  $ openFile traceFile AppendMode
      modify $ \s -> s{M.traceFileHandle = Just newHandle}
    Just handle <- gets M.traceFileHandle
    liftIO $ mapM_ (hPutStrLn handle) lg
  return v

memRead :: AccessType -> Address -> MinipsST (Word32, Latency)
memRead aType addr = loggingDoAccess =<< state (M.memRead aType addr)

readString :: Address -> MinipsST  (String, Latency)
readString memAddr = readStringAligned alignedAddr offset
  where
    alignedAddr = memAddr .&. 0xfffffffc
    offset      = fromIntegral $ memAddr .&. 0x00000003

readStringAligned :: Address -> Int -> MinipsST (String, Latency)
readStringAligned address offset = do
  (ws, lat0) <- first  breakWord <$> memRead Data address
  let (str0, ts) = first (map chr) $ span (/= 0) (drop offset ws)
  if null ts then do
    (str1, lat1) <- readStringAligned (address + 4) 0
    return (str0 <> str1, lat0 + lat1)
  else
    return (str0, lat0)

memWrite ::  Address -> Word32 -> MinipsST Latency
memWrite addr val = loggingDoAccess =<< state (M.memWrite addr val)

getMemStats :: MinipsST [(String, AccessStats)]
getMemStats = gets M.getMemStats
