module Emulator where

import Architecture
import RuntimeST
import Utils

import Data.Bits
import Data.Int
import Control.Monad
import Control.Monad.State.Strict
import Data.Word
import Data.Char

import System.IO

countInstruction :: InstrWord -> MinipsST ()
countInstruction RInstr{} = incRICount
countInstruction IInstr{} = incIICount
countInstruction JInstr{} = incJICount
countInstruction Syscall  = incRICount

runInstruction :: InstrWord -> MinipsST ()
runInstruction i = do
  countInstruction i
  runInstruction' i

runInstruction' :: InstrWord -> MinipsST ()
runInstruction' (RInstr i rs rt rd sa) = do
  incPC
  rsv <- regRead rs
  rtv <- regRead rt
  case i of
    ADD ->
      rd !< rsv + rtv
    ADDU ->
      rd !< rsv + rtv
    AND ->
      rd !< rsv .&. rtv
    JR ->
      Pc !< (fromIntegral rsv :: Word32)
    NOR ->
      rd !< complement (rsv .|. rtv)
    OR ->
      rd !< rsv .|. rtv
    SLT ->
      rd !< if (fromIntegral rsv :: Int32) < fromIntegral rtv then 1 else 0 :: Word32
    SLTU ->
      rd !< if (fromIntegral rsv :: Word32) < fromIntegral rtv then 1 else 0 :: Word32
    SLL ->
      rd !< rtv `shiftL` fromIntegral sa
    SRL ->
      rd !< rtv `shiftR` fromIntegral sa
    SUB ->
      rd !< rsv - rtv
    SUBU ->
      rd !< ((fromIntegral rsv  - fromIntegral rsv)  :: Word32)
    _    ->
      error "Unknown R-Type Instruction"
runInstruction' (IInstr i rs rt im) = do
  incPC
  rsv <- regRead rs
  rtv <- regRead rt
  pcv <- regRead Pc
  case i of
    ADDI ->
      rt !< rsv + signExtend im
    ADDIU ->
      rt !< rsv + signExtend im
    ANDI ->
      rt !< rsv .&. zeroExtend im
    BEQ ->
      when (rsv == rtv) $ Pc !<  pcv + (signExtend im `shiftL` 2)
    BNE ->
      when (rsv /= rtv) $ Pc !<  pcv + (signExtend im `shiftL` 2)
    LBU ->
      liftIO $ notImplemented "LBU"
    LHU ->
      liftIO $ notImplemented "LHU"
    LUI ->
      rt !< (zeroExtend im `shiftL` 16 :: Word32)
    LW -> do
      mval <- memRead (rsv + signExtend im)
      rt !< mval
    ORI ->
      rt !< fromIntegral rsv .|. (fromIntegral im :: Word32)
    SLTI ->
      rt !< if (fromIntegral rsv :: Int32) < signExtend im
            then 1
            else 0 :: Word32
    SLTIU ->
      rt !< if (fromIntegral rsv :: Word32) < signExtend im
            then 1
            else 0 :: Word32
    SB ->
      liftIO $ notImplemented "SB"
    SH ->
      liftIO $ notImplemented "SH"
    SW ->
      memWrite (rsv + signExtend im) rtv
    _    ->
      error "Unknown R-Type Instruction"
  return ()
runInstruction' (JInstr i addr) = do
  let adShifted = addr `shiftL` 2
  case i of
    J   -> do
      Pc !< adShifted
    JAL -> do
      pcv <- regRead Pc
      Ra  !< pcv + 4
      Pc  !< adShifted
    _   ->
      error "Unknown J-Type Instrucion"
runInstruction' Syscall = do
  incPC
  funct <- regRead V0
  a0v   <- regRead A0
  case funct of
    1  ->
      -- print int in $a0
      liftIO $ putStr (show a0v)
    2  ->
      -- print float in $f12
      liftIO $ notImplemented "SYSCALL 2"
    3  ->
      -- print double in $f13
      liftIO $ notImplemented "SYSCALL 3"
    4  -> do
      -- print string at addr $a0
      str <- readString a0v
      liftIO $ putStr str
    5  -> do
      -- read integer into $v0
      int <- liftIO (readLn :: IO Int32)
      V0 !<  int
    6  -> -- read float into $f0
      liftIO $ notImplemented "SYSCALL 6"
    7  -> -- read double into $f0
      liftIO $ notImplemented "SYSCALL 7"
    8  -> -- read string: $a0 = addr of input buffer $a1 = maximum number of bytes to read
      liftIO $ notImplemented "SYSCALL 8"
    9  -> -- alocate heap memory $a0 = bytes to allocate, $v0 addr of allocated memory
      liftIO $ notImplemented "SYSCALL 9"
    10 -> do -- exit with success
        liftIO $ glog "Execution finished successfully"
        Hlt !< (1 :: Word32)
    11 -> -- print character in $a0
      liftIO $ putChar $ chr (fromIntegral (a0v .&. 0xFF))
    12 -> -- reads character into $v0
      liftIO $ notImplemented "SYSCALL 12"
    _  -> error ("Unknown syscall: $a0=0x" ++ showHex a0v " v0=0x" ++ showHex funct "")

runLoop :: MinipsST ICount
runLoop = do
  regRead Pc >>=
    memRead >>=
    decodeInstruction >>=
    runInstruction
  hltv <- regRead Hlt
  if hltv == 1
    then getCounts
    else runLoop

simulate :: Endianness -> [Word32] -> [Word32] -> IO ICount
simulate end txt dt = do
  hSetBuffering stdout NoBuffering
  let iniMinips = makeMinips end txt dt
  -- putStrLn $ prettyPrint iniMinips
  evalStateT runLoop iniMinips
