module RuntimeST (
      module RuntimeST
    , AccessType(..)
    , MemoryAccessResponse
    , MemoryHierarchy
    , Log
    , Latency
    , R.ICount
    , R.rTypeICount, R.iTypeICount, R.jTypeICount
    , R.traceFileHandle
    ) where

import Architecture
import qualified Runtime as R
import Utils
import MemoryHierarchy

import Data.Word as W

import Control.Monad.State
import Data.Binary.IEEE754
import qualified Data.IntMap as IM
import Constants
import Text.Printf
import Data.Function
import Data.Bits
import Data.Bifunctor
import Data.Char
import System.IO
import Data.Maybe

type MinipsST = StateT R.Minips IO


------------
------------
-- Minips --
------------
------------

makeMinips :: Executable -> MemoryHierarchy -> Either String R.Minips
makeMinips (txt, ro, dt) memHier
  | length txt > maxTextSize @W32 = Left $
      printf ".text section overlaps with .rodata. Maximum .text size %d MB." $
             maxTextSize @Mebi
  | length ro > maxRodataSize @W32  = Left $
      printf ".rodata section overlaps with .data. Maximum .rodata size %.2f MB." $
             maxRodataSize @Mebi
  | otherwise = Right $ R.Minips flushedMemHier regs fpregs (0, 0, 0, 0) 0 Nothing Nothing
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
tick = modify R.tick

getStats :: MinipsST (Int, R.ICount)
getStats = do
  cycles <- gets R.cycles
  counts <- gets R.iCount
  return (cycles, counts)

getBranchDelaySlotAddress :: MinipsST (Maybe Word32)
getBranchDelaySlotAddress = gets R.delaySlotAddr

setBranchDelaySlotAddress :: Maybe Word32 -> MinipsST ()
setBranchDelaySlotAddress = modify . R.setBranchDelaySlotAddress

resetBranchDelaySlotAddress :: MinipsST ()
resetBranchDelaySlotAddress = setBranchDelaySlotAddress Nothing

decodeInstruction :: Word32 -> MinipsST InstrWord
decodeInstruction = state . R.decodeInstruction

incPC :: MinipsST ()
incPC = modify R.incPC

incRICount, incIICount, incJICount, incFRCount :: MinipsST ()
incRICount = modify R.incRICount
incIICount = modify R.incIICount
incJICount = modify R.incJICount
incFRCount = modify R.incFRCount

----------
----------
-- GPRs --
----------
----------

regRead :: RegName -> MinipsST Word32
regRead = gets . R.regRead

regWrite :: Integral32 a => RegName -> a -> MinipsST ()
regWrite r = modify . R.regWrite r

infixr 4 !<
(!<) :: Integral32 a => RegName -> a -> MinipsST ()
(!<) = regWrite

----------
----------
-- FPRs --
----------
----------

fpRegRead :: FPRegName -> MinipsST Word32
fpRegRead = gets . R.fpRegRead

fpRegReadFloat :: FPRegName -> MinipsST Float
fpRegReadFloat reg = wordToFloat <$> fpRegRead reg

fpRegReadDouble :: FPRegName -> MinipsST Double
fpRegReadDouble reg = do
  low <- fpRegRead reg
  up <- fpRegRead (succ reg)
  return $ doubleFromWords32 up low

fpRegWrite :: Integral32 a => FPRegName -> a -> MinipsST ()
fpRegWrite r = modify . R.fpRegWrite r

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
    whenM (gets $ isNothing . R.traceFileHandle) $ do
      newHandle <- liftIO  $ openFile traceFile AppendMode
      modify $ \s -> s{R.traceFileHandle = Just newHandle}
    Just handle <- gets R.traceFileHandle
    liftIO $ hPutStr handle (unlines lg)
  return v

memRead :: AccessType -> Address -> MinipsST (Word32, Latency)
memRead aType addr = loggingDoAccess =<< state (R.memRead aType addr)

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
memWrite addr val = loggingDoAccess =<< state (R.memWrite addr val)
