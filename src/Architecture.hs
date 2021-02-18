{-# LANGUAGE GADTs #-}

module Architecture where

import Data.List

import Data.Bits
import Data.Int
import Data.Word
import Utils

data Endianness = Big | Little deriving Eq

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
  | Hlt
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
      Hlt -> "$hlt"

w2reg :: Word8 -> RegName
w2reg = toEnum . fromIntegral

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

type Shamt     = Word8

data InstrWord where
  RInstr  :: Instr -> RegName -> RegName -> RegName -> Shamt -> InstrWord
  IInstr  :: Instr -> RegName -> RegName -> Int16            -> InstrWord
  JInstr  :: Instr -> Word32                                 -> InstrWord
  Syscall ::                                                    InstrWord

data ShowBox = forall s. Show s => Bx s
instance Show ShowBox where
  show (Bx s) = show s

showBox :: InstrWord -> [ShowBox]
showBox (RInstr JR rs _  _  _)   = [Bx JR,  Bx rs]
showBox (RInstr SLL _ rt rd sa)  = [Bx SLL, Bx rd, Bx rt, Bx sa]
showBox (RInstr SRL _ rt rd sa)  = [Bx SRL, Bx rd, Bx rt, Bx sa]
showBox (RInstr i  rs rt rd _)   = [Bx i,   Bx rd, Bx rs, Bx rt]

showBox (IInstr LUI _  rt w)     = [Bx LUI, Bx rt, Bx w]
showBox (IInstr BEQ rs rt w)     = [Bx BEQ, Bx rs, Bx rt, Bx w]
showBox (IInstr BNE rs rt w)     = [Bx BNE, Bx rs, Bx rt, Bx w]
showBox (IInstr SB  rs rt w)     = [Bx SB,  Bx rs, Bx rt, Bx w]
showBox (IInstr SH  rs rt w)     = [Bx SH,  Bx rs, Bx rt, Bx w]
showBox (IInstr i   rs rt w)     = [Bx i,   Bx rt, Bx rs, Bx w]

showBox _ = []

instance Show InstrWord where
  show (JInstr i w) = let w' = w `shiftL` 2 in show i <> " 0x" <> showHex w " # 0x" <> showHex w' ""
  show (IInstr SW rs rt w) = show SW <> " " <> show rt <> ", 0x" <> showHex w "(" <> show rs <> ")"
  show (IInstr LW rs rt w) = show LW <> " " <> show rt <> ", 0x" <> showHex w "(" <> show rs <> ")"
  show Syscall = "SYSCALL"
  show iw = ins <> " " <> intercalate ", " ops
    where
      (ins:ops) = show <$> showBox iw
