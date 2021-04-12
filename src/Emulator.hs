module Emulator where

import Architecture
import RuntimeST
import Utils

import Control.Monad
import Control.Monad.State.Strict
import Data.Functor
import Data.Maybe

import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import GHC.Float

import System.IO

countInstruction :: InstrWord -> MinipsST ()
countInstruction RInstr{}  = incRICount
countInstruction IInstr{}  = incIICount
countInstruction JInstr{}  = incJICount
countInstruction FRInstr{} = incFRCount
countInstruction Syscall   = incRICount

runInstruction :: InstrWord -> MinipsST Latency
runInstruction i = do
  countInstruction i
  ifM (isJust <$> getBranchDelaySlotAddress)
    -- then
    resetBranchDelaySlotAddress
    -- else
    incPC
  runInstruction' i

runInstruction' :: InstrWord -> MinipsST Latency
runInstruction' (RInstr i rs rt rd sa) = do
  rsv <- regRead rs
  rtv <- regRead rt
  case i of
    ADD  -> do
      rd !< rsv + rtv
      return 1
    ADDU -> do
      rd !< rsv + rtv
      return 1
    AND  -> do
      rd !< rsv .&. rtv
      return 1
    DIV  -> do
      let (q, r) = (fromIntegral rsv :: Int32) `quotRem` fromIntegral rtv
      Lo !< q
      Hi !< r
      return 1
    JALR -> do
      pcv <- regRead Pc
      setBranchDelaySlotAddress $ Just pcv
      rd !< pcv + 4
      Pc !< rsv
      return 1
    JR   -> do
      regRead Pc >>= setBranchDelaySlotAddress . Just
      Pc !< (fromIntegral rsv :: Word32)
      return 1
    MFLO -> do
      lo <- regRead Lo
      rd !< lo
      return 1
    MFHI -> do
      hi <- regRead Hi
      rd !< hi
      return 1
    MULT -> do
      let mult = (fromIntegral rsv :: Int64) * fromIntegral rtv
      Hi !< (fromIntegral (mult `shiftR` 32) :: Word32)
      Lo !< (fromIntegral (mult .&. 0xffffffff) :: Word32)
      return 1
    NOR  -> do
      rd !< complement (rsv .|. rtv)
      return 1
    OR   -> do
      rd !< rsv .|. rtv
      return 1
    SLT  -> do
      rd !< if (fromIntegral rsv :: Int32) < fromIntegral rtv then 1 else 0 :: Word32
      return 1
    SLTU -> do
      rd !< if (fromIntegral rsv :: Word32) < fromIntegral rtv then 1 else 0 :: Word32
      return 1
    SLL  -> do
      rd !< rtv `shiftL` fromIntegral sa
      return 1
    SRA  -> do
      rd !< (fromIntegral rtv :: Int32) `shiftR` fromIntegral sa
      return 1
    SRL  -> do
      rd !< rtv `shiftR` fromIntegral sa
      return 1
    SUB  -> do
      rd !< rsv - rtv
      return 1
    SUBU -> do
      rd !< ((fromIntegral rsv  - fromIntegral rsv)  :: Word32)
      return 1
    XOR  -> do
      rd !< rsv `xor` rtv
      return 1
    _    ->
      error $ "R-Type Instruction not implemented: " ++ pshow i
runInstruction' (IInstr i rs rt im) = do
  rsv <- regRead rs
  rtv <- regRead rt
  pcv <- regRead Pc
  case i of
    ADDI -> do
      rt !< rsv + signExtend im
      return 1
    ADDIU -> do
      rt !< rsv + signExtend im
      return 1
    ANDI -> do
      rt !< rsv .&. zeroExtend im
      return 1
    BEQ -> do
      when (rsv == rtv) $ do
        setBranchDelaySlotAddress $ Just pcv
        Pc !<  pcv + (signExtend im `shiftL` 2)
      return 1
    BGEZ -> do
       when ((fromIntegral rsv :: Int32) >= 0) $ do
         setBranchDelaySlotAddress $ Just pcv
         Pc !<  pcv + (signExtend im `shiftL` 2)
       return 1
    BLEZ -> do
       when ((fromIntegral rsv :: Int32) <= 0) $ do
         setBranchDelaySlotAddress $ Just pcv
         Pc !<  pcv + (signExtend im `shiftL` 2)
       return 1
    BNE -> do
      when (rsv /= rtv) $ do
        setBranchDelaySlotAddress $ Just pcv
        Pc !<  pcv + (signExtend im `shiftL` 2)
      return 1
    LB  -> do
      (val0, lat) <- memRead Data (rsv + signExtend im)
      let mvalw32 = val0 .&. 0xff
          mvali8  = fromIntegral mvalw32 :: Int8   -- Sinalizado, 8bits
          mvali32 = fromIntegral mvali8  :: Int32  -- Estende sinalizado 32
      rt !< mvali32
      return lat
    LBU -> do
      (mval, lat) <- memRead Data (rsv + signExtend im)
      rt !<  mval .&. 0xff
      return lat
    LHU -> do
      (mval, lat) <- memRead Data (rsv + signExtend im)
      rt !<  mval .&. 0xffff
      return lat
    LUI -> do
      rt !< (zeroExtend im `shiftL` 16 :: Word32)
      return 1
    LW -> do
      (mval, lat) <- memRead Data (rsv + signExtend im)
      rt !< mval
      return lat
    LDC1 -> do
      let fpreg0 = toEnum (fromEnum rt)
          fpreg1 = succ fpreg0
          addr   = rsv + signExtend im
      (mval, lat0) <- memRead Data addr
      fpreg0 !!< mval
      (mval2, lat1) <- memRead Data (addr + 4)
      fpreg1 !!< mval2
      return $ lat0 + lat1
    LWC1 -> do
      let fpreg = toEnum (fromEnum rt)
      (mval, lat) <- memRead Data (rsv + signExtend im)
      fpreg !!< mval
      return lat
    ORI -> do
      rt !< fromIntegral rsv .|. (fromIntegral im :: Word32)
      return 1
    SLTI -> do
      rt !< if (fromIntegral rsv :: Int32) < signExtend im
            then 1
            else 0 :: Word32
      return 1
    SLTIU -> do
      rt !< if (fromIntegral rsv :: Word32) < signExtend im
            then 1
            else 0 :: Word32
      return 1
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
runInstruction' (JInstr i addr) = do
  pcv <- regRead Pc
  let jmpAddress = (pcv .&. 0xf0000000) + (addr `shiftL` 2)
  case i of
    J   -> do
      setBranchDelaySlotAddress $ Just pcv
      Pc !< jmpAddress
      return 1
    JAL -> do
      setBranchDelaySlotAddress $ Just pcv
      Ra !< pcv + 4
      Pc !< jmpAddress
      return 1
    _   ->
      error $ "J-Type Instrucion not implemented: " ++ pshow i

runInstruction' (FRInstr i ft fs fd) =
  case i of
    ADDS  -> do
      fsv <- fpRegReadFloat fs
      ftv <- fpRegReadFloat ft
      fd `fpRegWriteFloat` (fsv + ftv)
      return 1
    ADDD  -> do
      d0 <- fpRegReadDouble fs
      d1 <- fpRegReadDouble ft
      fd `fpRegWriteDouble` (d0 + d1)
      return 1
    CVTDS -> do
      fsv <- fpRegReadFloat fs
      fd `fpRegWriteDouble` float2Double fsv
      return 1
    CVTDW -> do
      fsv <- fpRegRead fs
      let dv = fromIntegral (fromIntegral fsv :: Int32)
      fd `fpRegWriteDouble` dv
      return 1
    CVTSD -> do
      fsv <- fpRegReadDouble fs
      let fv = double2Float fsv
      fd `fpRegWriteFloat` fv
      return 1
    CVTSW -> do
      fsv <- fpRegRead fs
      let fv = fromIntegral (fromIntegral fsv :: Int32) :: Float
      fd `fpRegWriteFloat` fv
      return 1
    DIVS  -> do
      fsv <- fpRegReadFloat fs
      ftv <- fpRegReadFloat ft
      fd `fpRegWriteFloat` (fsv / ftv)
      return 1
    DIVD  -> do
      fsv <- fpRegReadDouble fs
      ftv <- fpRegReadDouble ft
      fd `fpRegWriteDouble` (fsv / ftv)
      return 1
    MFC1  -> do
      fsv <- fpRegRead fs
      rt !< fsv
      return 1
    MTC1  -> do
      rtv <- regRead rt
      fs !!< rtv
      return 1
    MOVS  -> do
      fsv <- fpRegRead fs
      fd !!< fsv
      return 1
    MOVD  -> do
      fsv0 <- fpRegRead fs
      fsv1 <- fpRegRead (succ fs)
      fd !!< fsv0
      succ fd !!< fsv1
      return 1
    MULS  -> do
      fsv <- fpRegReadFloat fs
      ftv <- fpRegReadFloat ft
      fd `fpRegWriteFloat` (fsv * ftv)
      return 1
    MULD -> do
      fsv <- fpRegReadDouble fs
      ftv <- fpRegReadDouble ft
      fd `fpRegWriteDouble` (fsv * ftv)
      return 1
    _     ->
      error $ "FR-Type Instrucion not implemented: " ++ pshow i
  where
    rt = toEnum (fromEnum ft) :: RegName

runInstruction' Syscall = do
  funct <- regRead V0
  a0v   <- regRead A0
  case funct of
    1  -> do
      -- print int in $a0
      liftIO $ putStr (show (fromIntegral a0v :: Int32))
      return 1
    2  -> do
      -- print float in $f12
      f12v <- fpRegReadFloat F12
      liftIO $ putStr (show f12v)
      return 1
    3  -> do
      -- print double in $f12/$f13
      d0 <- fpRegReadDouble F12
      liftIO $ putStr (show d0)
      return 1
    4  -> do
      -- print string at addr $a0
      (str, lat) <- readString a0v
      liftIO $ putStr str
      return lat
    5  -> do
      -- read integer into $v0
      int <- liftIO (readLn :: IO Int32)
      V0 !<  int
      return 1
    6  -> do
      -- read float into $f0
      fl <- liftIO (readLn :: IO Float)
      F0 `fpRegWriteFloat` fl
      return 1
    7  -> do
      -- read double into $f0
      db <- liftIO (readLn :: IO Double)
      F0 `fpRegWriteDouble` db
      return 1
    8  -> do -- read string: $a0 = addr of input buffer $a1 = maximum number of bytes to read
      liftIO $ notImplemented "SYSCALL 8"
      return 0
    9  -> do -- alocate heap memory $a0 = bytes to allocate, $v0 addr of allocated memory
      liftIO $ notImplemented "SYSCALL 9"
      return 0
    10 -> do -- exit with success
        liftIO $ glog "\nExecution finished successfully"
        Hlt !< (1 :: Word32)
        return 1
    11 -> do -- print character in $a0
      liftIO $ putChar $ chr (fromIntegral (a0v .&. 0xFF))
      return 1
    12 -> do -- reads character into $v0
      liftIO $ notImplemented "SYSCALL 12"
      return 1
    _  -> error ("Unknown syscall: $a0=0x" ++ showHex a0v " v0=0x" ++ showHex funct "")

runLoop :: MinipsST (Int, ICount)
runLoop = do
  mHandle <- gets traceFileHandle
  liftIO $ maybe (pure ()) hFlush mHandle

  tick
  dsa <- getBranchDelaySlotAddress
  pcv <- regRead Pc
  (inst, lat0) <- memRead Instruction (fromMaybe pcv dsa)
  lat1 <- decodeInstruction inst >>= runInstruction
  ifM (regRead Hlt <&> (== 1))
    finalizeExecution
    runLoop
  where
    finalizeExecution = do
      mHandle <- gets traceFileHandle
      liftIO $ maybe (pure ()) hFlush mHandle
      getStats

simulate :: ([Word32], [Word32], [Word32]) -> MemoryHierarchy -> IO (Int, ICount)
simulate exe mh = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  either
    error
    (evalStateT runLoop)
    (makeMinips exe mh)
