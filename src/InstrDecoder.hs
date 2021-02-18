module InstrDecoder (disassemble, decodeInstruction) where

import Architecture
import Utils
import Data.Bits
import Data.Int
import Data.Word

disassemble :: [Word32] -> String
disassemble ws = unlines $ map (show . decodeInstruction) ws

decodeInstruction :: Word32 -> InstrWord
decodeInstruction w =
  case getOpcode w of
    -- R-Type
    0x00 -> decodeRType w
    -- J-type
    0x02 -> decodeJType w
    0x03 -> decodeJType w
    -- I-type
    _    -> decodeIType w


decodeRType :: Word32 -> InstrWord
decodeRType w =
  ins
  where
    ins = if funct == 0x0c
          then Syscall
          else RInstr instr (getRsR w) (getRtR w) (getRdR w) (getShamt w)
    funct = getFunct w
    instr = case funct of
      0x20 -> ADD
      0x21 -> ADDU
      0x24 -> AND
      0x08 -> JR
      0x27 -> NOR
      0x25 -> OR
      0x2a -> SLT
      0x2b -> SLTU
      0x00 -> SLL
      0x02 -> SRL
      0x22 -> SUB
      0x23 -> SUBU
      _    -> error $  "Instrução tipo R não reconhecida. Funct: " ++ show funct ++ " Word: " ++ showHex w ""

decodeJType :: Word32 -> InstrWord
decodeJType w
  | op == 0x02 = JInstr J ad
  | op == 0x03 = JInstr JAL ad
  | otherwise  = error $  "decodeJType com palavra: " ++ show w
  where
    op = getOpcode w
    ad = getAddress w

decodeIType :: Word32 -> InstrWord
decodeIType w =
  ins
  where
    ins = IInstr instr (getRsR w) (getRtR w) (getImmediate w)
    op = getOpcode w
    instr = case op of
      0x08 -> ADDI
      0x09 -> ADDIU
      0x0c -> ANDI
      0x04 -> BEQ
      0x05 -> BNE
      0x24 -> LBU
      0x25 -> LHU
      0x0f -> LUI
      0x23 -> LW
      0x0d -> ORI
      0x0a -> SLTI
      0x0b -> SLTIU
      0x28 -> SB
      0x29 -> SH
      0x2b -> SW
      _    ->
        error $ "Instrução tipo I não reconhecida. Opcode: " ++ show op ++ " Word: 0x" ++ showHex w ""

getOpcode, getRs, getRt, getRd, getShamt, getFunct :: Word32 -> Word8
getOpcode    w = fromIntegral $ (w .&. 0xfc000000) `shiftR` 26
getRs        w = fromIntegral $ (w .&. 0x03e00000) `shiftR` 21
getRt        w = fromIntegral $ (w .&. 0x001f0000) `shiftR` 16
getRd        w = fromIntegral $ (w .&. 0x0000f800) `shiftR` 11
getShamt     w = fromIntegral $ (w .&. 0x000007c0) `shiftR` 6
getFunct     w = fromIntegral $ w .&. 0x0000003f

getAddress :: Word32 -> Word32
getAddress w = w .&. 0x03ffffff

getImmediate :: Word32 -> Int16
getImmediate w = fromIntegral v
  where
     v = (fromIntegral w .&. 0x0000ffff) :: Int32

getRsR, getRtR, getRdR :: Word32 -> RegName
getRsR = w2reg . getRs
getRtR = w2reg . getRt
getRdR = w2reg . getRd
