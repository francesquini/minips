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
import Data.Maybe (fromMaybe, isJust)
import GHC.Float
import Data.Functor

countInstruction :: InstrWord -> MinipsST ()
countInstruction RInstr{}  = incRICount
countInstruction IInstr{}  = incIICount
countInstruction JInstr{}  = incJICount
countInstruction FRInstr{} = incFRCount
countInstruction Syscall   = incRICount

runInstruction :: InstrWord -> MinipsST ()
runInstruction i = do
  countInstruction i
  ifM (isJust <$> getBranchDelaySlotAddress)
    -- then
    resetBranchDelaySlotAddress
    -- else
    incPC
  runInstruction' i

runInstruction' :: InstrWord -> MinipsST ()
runInstruction' (RInstr i rs rt rd sa) = do
  rsv <- regRead rs
  rtv <- regRead rt
  case i of
    ADD  ->
      rd !< rsv + rtv
    ADDU ->
      rd !< rsv + rtv
    AND  ->
      rd !< rsv .&. rtv
    DIV  -> do
      let (q, r) = (fromIntegral rsv :: Int32) `quotRem` fromIntegral rtv
      Lo !< q
      Hi !< r
    JALR -> do
      pcv <- regRead Pc
      setBranchDelaySlotAddress $ Just pcv
      rd !< pcv + 4
      Pc !< rsv
    JR   -> do
      regRead Pc >>= setBranchDelaySlotAddress . Just
      Pc !< (fromIntegral rsv :: Word32)
    MFLO -> do
      lo <- regRead Lo
      rd !< lo
    MFHI -> do
      hi <- regRead Hi
      rd !< hi
    MULT -> do
      let mult = (fromIntegral rsv :: Int64) * fromIntegral rtv
      Hi !< (fromIntegral (mult `shiftR` 32) :: Word32)
      Lo !< (fromIntegral (mult .&. 0xffffffff) :: Word32)
    NOR  ->
      rd !< complement (rsv .|. rtv)
    OR   ->
      rd !< rsv .|. rtv
    SLT  ->
      rd !< if (fromIntegral rsv :: Int32) < fromIntegral rtv then 1 else 0 :: Word32
    SLTU ->
      rd !< if (fromIntegral rsv :: Word32) < fromIntegral rtv then 1 else 0 :: Word32
    SLL  ->
      rd !< rtv `shiftL` fromIntegral sa
    SRA  ->
      rd !< (fromIntegral rtv :: Int32) `shiftR` fromIntegral sa
    SRL  ->
      rd !< rtv `shiftR` fromIntegral sa
    SUB  ->
      rd !< rsv - rtv
    SUBU ->
      rd !< ((fromIntegral rsv  - fromIntegral rsv)  :: Word32)
    XOR  ->
      rd !< rsv `xor` rtv
    _    ->
      error $ "R-Type Instruction not implemented: " ++ pshow i
runInstruction' (IInstr i rs rt im) = do
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
      when (rsv == rtv) $ do
        setBranchDelaySlotAddress $ Just pcv
        Pc !<  pcv + (signExtend im `shiftL` 2)
    BGEZ ->
       when ((fromIntegral rsv :: Int32) >= 0) $ do
        setBranchDelaySlotAddress $ Just pcv
        Pc !<  pcv + (signExtend im `shiftL` 2)
    BLEZ ->
       when ((fromIntegral rsv :: Int32) <= 0) $ do
        setBranchDelaySlotAddress $ Just pcv
        Pc !<  pcv + (signExtend im `shiftL` 2)
    BNE ->
      when (rsv /= rtv) $ do
        setBranchDelaySlotAddress $ Just pcv
        Pc !<  pcv + (signExtend im `shiftL` 2)
    LB  -> do
      mvalw32 <- memRead (rsv + signExtend im) <&> (.&. 0xff)
      let mvali8 =   fromIntegral mvalw32 :: Int8   -- Sinalizado, 8bits
          mvali32 =  fromIntegral mvali8  :: Int32  -- Estende sinalizado 32
      rt !< mvali32
    LBU -> do
      mval <- memRead (rsv + signExtend im)
      rt !<  mval .&. 0xff
    LHU -> do
      mval <- memRead (rsv + signExtend im)
      rt !<  mval .&. 0xffff
    LUI ->
      rt !< (zeroExtend im `shiftL` 16 :: Word32)
    LW -> do
      mval <- memRead (rsv + signExtend im)
      rt !< mval
    LDC1 -> do
      let fpreg0 = toEnum (fromEnum rt)
          fpreg1 = succ fpreg0
          addr   = rsv + signExtend im
      mval <- memRead addr
      fpreg0 !!< mval
      mval2 <- memRead (addr + 4)
      fpreg1 !!< mval2
    LWC1 -> do
      let fpreg = toEnum (fromEnum rt)
      mval <- memRead (rsv + signExtend im)
      fpreg !!< mval
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
      memWrite (rsv + signExtend im) (rtv .&. 0xff)
    SH ->
      memWrite (rsv + signExtend im) (rtv .&. 0xffff)
    SW ->
      memWrite (rsv + signExtend im) rtv
    SWC1 -> do
      val <- fpRegRead $ toEnum (fromEnum rt)
      memWrite (rsv + signExtend im) val
    _    ->
      error $ "I-Type Instruction not implemented: " ++ pshow i
  return ()
runInstruction' (JInstr i addr) = do
  pcv <- regRead Pc
  let jmpAddress = (pcv .&. 0xf0000000) + (addr `shiftL` 2)
  case i of
    J   -> do
      setBranchDelaySlotAddress $ Just pcv
      Pc !< jmpAddress
    JAL -> do
      setBranchDelaySlotAddress $ Just pcv
      Ra !< pcv + 4
      Pc !< jmpAddress
    _   ->
      error $ "J-Type Instrucion not implemented: " ++ pshow i

runInstruction' (FRInstr i ft fs fd) =
  case i of
    ADDS  -> do
      fsv <- fpRegReadFloat fs
      ftv <- fpRegReadFloat ft
      fd `fpRegWriteFloat` (fsv + ftv)
    ADDD  -> do
      d0 <- fpRegReadDouble fs
      d1 <- fpRegReadDouble ft
      fd `fpRegWriteDouble` (d0 + d1)
    CVTDS -> do
      fsv <- fpRegReadFloat fs
      fd `fpRegWriteDouble` float2Double fsv
    CVTDW -> do
      fsv <- fpRegRead fs
      let dv = fromIntegral (fromIntegral fsv :: Int32)
      fd `fpRegWriteDouble` dv
    CVTSD -> do
      fsv <- fpRegReadDouble fs
      let fv = double2Float fsv
      fd `fpRegWriteFloat` fv
    CVTSW -> do
      fsv <- fpRegRead fs
      let fv = fromIntegral (fromIntegral fsv :: Int32) :: Float
      fd `fpRegWriteFloat` fv
    DIVS  -> do
      fsv <- fpRegReadFloat fs
      ftv <- fpRegReadFloat ft
      fd `fpRegWriteFloat` (fsv / ftv)
    DIVD  -> do
      fsv <- fpRegReadDouble fs
      ftv <- fpRegReadDouble ft
      fd `fpRegWriteDouble` (fsv / ftv)
    MFC1  -> do
      fsv <- fpRegRead fs
      rt !< fsv
    MTC1  -> do
      rtv <- regRead rt
      fs !!< rtv
    MOVS  -> do
      fsv <- fpRegRead fs
      fd !!< fsv
    MOVD  -> do
      fsv0 <- fpRegRead fs
      fsv1 <- fpRegRead (succ fs)
      fd !!< fsv0
      succ fd !!< fsv1
    MULS  -> do
      fsv <- fpRegReadFloat fs
      ftv <- fpRegReadFloat ft
      fd `fpRegWriteFloat` (fsv * ftv)
    MULD -> do
      fsv <- fpRegReadDouble fs
      ftv <- fpRegReadDouble ft
      fd `fpRegWriteDouble` (fsv * ftv)
    _     ->
      error $ "FR-Type Instrucion not implemented: " ++ pshow i
  where
    rt = toEnum (fromEnum ft) :: RegName

runInstruction' Syscall = do
  funct <- regRead V0
  a0v   <- regRead A0
  case funct of
    1  ->
      -- print int in $a0
      liftIO $ putStr (show (fromIntegral a0v :: Int32))
    2  -> do
      -- print float in $f12
      f12v <- fpRegReadFloat F12
      liftIO $ putStr (show f12v)
    3  -> do
      -- print double in $f12/$f13
      d0 <- fpRegReadDouble F12
      liftIO $ putStr (show d0)
    4  -> do
      -- print string at addr $a0
      str <- readString a0v
      liftIO $ putStr str
    5  -> do
      -- read integer into $v0
      int <- liftIO (readLn :: IO Int32)
      V0 !<  int
    6  -> do
      -- read float into $f0
      fl <- liftIO (readLn :: IO Float)
      F0 `fpRegWriteFloat` fl
    7  -> do
      -- read double into $f0
      db <- liftIO (readLn :: IO Double)
      F0 `fpRegWriteDouble` db
    8  -> -- read string: $a0 = addr of input buffer $a1 = maximum number of bytes to read
      liftIO $ notImplemented "SYSCALL 8"
    9  -> -- alocate heap memory $a0 = bytes to allocate, $v0 addr of allocated memory
      liftIO $ notImplemented "SYSCALL 9"
    10 -> do -- exit with success
        liftIO $ glog "\nExecution finished successfully"
        Hlt !< (1 :: Word32)
    11 -> -- print character in $a0
      liftIO $ putChar $ chr (fromIntegral (a0v .&. 0xFF))
    12 -> -- reads character into $v0
      liftIO $ notImplemented "SYSCALL 12"
    _  -> error ("Unknown syscall: $a0=0x" ++ showHex a0v " v0=0x" ++ showHex funct "")

runLoop :: MinipsST (Int, ICount)
runLoop = do
  tick
  dsa <- getBranchDelaySlotAddress
  pcv <- regRead Pc
  memRead (fromMaybe pcv dsa) >>=
    decodeInstruction >>=
    runInstruction
  hltv <- regRead Hlt
  if hltv == 1
    then getStats
    else runLoop

simulate :: Endianness -> ([Word32], [Word32], [Word32]) -> IO (Int, ICount)
simulate end exe = do
  hSetBuffering stdout NoBuffering
  let iniMinips = makeMinips end exe
  evalStateT runLoop iniMinips
