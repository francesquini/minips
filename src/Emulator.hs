module Emulator where

import Architecture
import RuntimeST
import Utils

import Data.Bits
import Control.Monad
import Control.Monad.State.Strict
import Data.Word
import Data.Char
import Debug.Trace
import Numeric

runInstruction :: InstrWord -> MinipsSt (IO ())
runInstruction (RInstr i rs rt rd sa) = do
  incPC
  rsv <- regVal rs
  rtv <- regVal rt
  case i of
    ADD ->
      rd !< rsv + rtv
    ADDU ->
      rd !< ((fromIntegral rsv  + fromIntegral rsv)  :: Word32)
    AND ->
      rd !< rsv .&. rtv
    JR ->
      Pc !< (fromIntegral rsv :: Word32)
    NOR ->
      rd !< complement (rsv .|. rtv)
    OR ->
      rd !< rsv .|. rtv
    SLT ->
      rd !< if rsv < rtv then 1 else 0 :: Word32
    SLTU ->
      rd !< if (fromIntegral rsv :: Word32) < (fromIntegral rtv :: Word32) then 1 else 0 :: Word32
    SLL ->
      rd !< rtv `shiftL` fromIntegral sa
    SRL -> undefined
      rd !< rtv `shiftR` fromIntegral sa
    SUB ->
      rd !< rsv - rtv
    SUBU ->
      rd !< ((fromIntegral rsv  - fromIntegral rsv)  :: Word32)
    _    ->
      error "Unknown R-Type Instruction"
  return $ return ()
runInstruction (IInstr i rs rt im) = do
  incPC
  rsv <- regVal rs
  rtv <- regVal rt
  pcv <- regVal Pc
  case i of
    ADDI ->
      rt !< rsv + signExtend im
    ADDIU ->
      rt !< rsv + signExtend im
    ANDI ->
      rt !< rsv .&. zeroExtend im
    BEQ ->
      when (rsv == rtv) $ Pc !<  pcv + zeroExtend im
    BNE ->
      when (rsv /= rtv) $ Pc !<  pcv + zeroExtend im
    LBU -> undefined
    LHU -> undefined
    LUI ->
      rt !< (zeroExtend im `shiftL` 16 :: Word32)
    LW -> undefined
    ORI -> undefined
      rt !< fromIntegral rsv .|. (fromIntegral im :: Word32)
    SLTI ->
      rt !< if rsv < signExtend im
            then 1
            else 0 :: Word32
    SLTIU ->
      rt !< if (fromIntegral rsv :: Word32) < (signExtend im :: Word32)
            then 1
            else 0 :: Word32
    SB -> undefined
    SH -> undefined
    SW -> undefined
    _    ->
      error "Unknown R-Type Instruction"
  return $ return ()
runInstruction (JInstr i ad) = do
  case i of
    J   ->
      Pc !< ad
    JAL -> do
      pcv <- regVal Pc
      Ra  !< pcv + 8
      Pc  !< ad
    _   ->
      error "Unknown J-Type Instrucion"
  return $ return ()
runInstruction Syscall = do
  incPC
  funct <- regVal V0
  a0v   <- regVal A0
  trace ("Running syscall. $a0=0x" ++ showHex a0v " v0=0x" ++ showHex funct "") (return ())
  return $ case funct of
    1  -> --print int in $a0
      print a0v
    10 -> do
      print "Execution finished successfully"
      error "Para tudo!"
    11 -> do
      print $ chr (fromIntegral (a0v .&. 0xFF))
    _  -> error ("Unknown syscall: $a0=0x" ++ showHex a0v " v0=0x" ++ showHex funct "")

runLoop :: Int -> MinipsSt (IO ())
runLoop 0 =   return $ return ()
runLoop n = do
  pcv <- regVal Pc
  x <- trace ("Executando PC: " ++ showHex pcv "") $
    memVal pcv >>= runInstruction . decodeInstr
  seq x runLoop (n - 1)

simulate :: [Word32] -> [Word32] -> IO ()
simulate txt dt = do
  let iniMinips = makeMinips txt dt
  print $ prettyPrint iniMinips
  evalState (runLoop 10) iniMinips
