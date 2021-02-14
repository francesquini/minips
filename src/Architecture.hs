{-# LANGUAGE GADTs #-}

module Architecture where

import Data.List

import Data.Bits
import Data.Int
import Data.Word
import Numeric

import Debug.Trace


data Endianness = Big | Little

data RegName =
  -- General purpose registers
    Zero
  | At
  | V0 | V1
  | A0 | A1 | A2 | A3
  | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
  | T8 | T9
  | K0 | K1
  | Gp
  | Sp
  | Fp
  | Ra
  -- Specific registers
  | Pc
  deriving (Enum)

instance Show RegName where
  show r =
    case r of
      Zero -> "$zero"
      At -> "$at"
      V0 -> "$v0"
      V1 -> "$v1"
      A0 -> "$a0"
      A1 -> "$a1"
      A2 -> "$a2"
      A3 -> "$a3"
      T0 -> "$t0"
      T1 -> "$t1"
      T2 -> "$t2"
      T3 -> "$t3"
      T4 -> "$t4"
      T5 -> "$t5"
      T6 -> "$t6"
      T7 -> "$t7"
      S0 -> "$s0"
      S1 -> "$s1"
      S2 -> "$s2"
      S3 -> "$s3"
      S4 -> "$s4"
      S5 -> "$s5"
      S6 -> "$s6"
      S7 -> "$s7"
      T8 -> "$t8"
      T9 -> "$t9"
      K0 -> "$k0"
      K1 -> "$k1"
      Gp -> "$gp"
      Sp -> "$sp"
      Fp -> "$fp"
      Ra -> "$ra"
      Pc -> "$pc"

w2reg :: Word8 -> RegName
w2reg = toEnum . fromIntegral

type Opcode = Word8
type Shamt  = Word8
type Funct  = Word8

data Instr =
  -- R-type
    ADD
  | ADDU
  | AND
  | JR
  | NOR
  | OR
  | SLT
  | SLTU
  | SLL
  | SRL
  | SUB
  | SUBU
  | SYSCALL
  -- I-type
  | ADDI
  | ADDIU
  | ANDI
  | BEQ
  | BNE
  | LBU
  | LHU
  | LUI
  | LW
  | ORI
  | SLTI
  | SLTIU
  | SB
  | SH
  | SW
  -- J-type
  | J
  | JAL
  deriving (Show)

data InstrWord where
  RInstr  :: Instr -> RegName -> RegName -> RegName -> Shamt -> InstrWord
  IInstr  :: Instr -> RegName -> RegName -> Int16            -> InstrWord
  JInstr  :: Instr -> Word32                                 -> InstrWord
  Syscall ::                                                    InstrWord

data ShowBox = forall s. Show s => Bx s
instance Show ShowBox where
  show (Bx s) = show s

showBox :: InstrWord -> [ShowBox]
showBox Syscall                  = []
showBox (RInstr JR rs _  _  _)   = [Bx JR,  Bx rs]
showBox (RInstr SLL _ rt rd sa)  = [Bx SLL, Bx rd, Bx rt, Bx sa]
showBox (RInstr SRL _ rt rd sa)  = [Bx SRL, Bx rd, Bx rt, Bx sa]
showBox (RInstr i  rs rt rd _)   = [Bx i,   Bx rd, Bx rs, Bx rt]

showBox (IInstr LUI _  rt w)     = [Bx LUI, Bx rt, Bx w]
showBox (IInstr BEQ rs rt w)     = [Bx BEQ, Bx rs, Bx rt, Bx w]
showBox (IInstr BNE rs rt w)     = [Bx BNE, Bx rs, Bx rt, Bx w]
showBox (IInstr SB  rs rt w)     = [Bx SB,  Bx rs, Bx rt, Bx w]
showBox (IInstr SH  rs rt w)     = [Bx SH,  Bx rs, Bx rt, Bx w]
showBox (IInstr SW  rs rt w)     = [Bx SH,  Bx rs, Bx rt, Bx w]
showBox (IInstr i   rs rt w)     = [Bx i,   Bx rt, Bx rs, Bx w]

showBox (JInstr i         w)     = [Bx i, Bx w]

instance Show InstrWord where
  show Syscall = "SYSCALL"
  show iw = ins <> " " <> intercalate ", " ops
    where
      (ins:ops) = show <$> showBox iw

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

decodeRType :: Word32 -> InstrWord
decodeRType w =
  trace ("decodeRType: " ++ show ins)
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
      _    -> error $  "Instrução tipo R não reconhecida. Funct: " ++ show funct

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
  trace ("decodeIType: " ++ show ins)
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
      _    -> error $ "Instrução tipo I não reconhecida. Opcode: " ++ show op

decodeInstr :: Word32 -> InstrWord
decodeInstr w =
  trace ("Decodificando instrução: 0x" ++ showHex w "") $
  case getOpcode w of
    -- R-Type
    0x00 -> decodeRType w
    -- J-type
    0x02 -> decodeJType w
    0x03 -> decodeJType w
    -- I-type
    _    -> decodeIType w
