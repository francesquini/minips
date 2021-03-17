{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Architecture where

import Data.List

import Data.Bits
import Data.Int
import Data.Word
import Utils
import Data.Char (toLower)

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
  | Hi | Lo
  deriving (Eq, Enum, Show)

data FPRegName =
  -- Coprocessor 1
    F0  | F1  | F2  | F3  | F4  | F5  | F6  | F7
  | F8  | F9  | F10 | F11 | F12 | F13 | F14 | F15
  | F16 | F17 | F18 | F19 | F20 | F21 | F22 | F23
  | F24 | F25 | F26 | F27 | F28 | F29 | F30 | F31
  deriving (Eq, Enum, Show)

w2reg :: Word8 -> RegName
w2reg = toEnum . fromIntegral

w2fpreg :: Word8 -> FPRegName
w2fpreg = toEnum . fromIntegral

data Instr =
  -- R-type
    ADD
  | ADDU
  | AND
  | BREAK
  | DIV
  | DIVU
  | JR
  | JALR
  | NOR
  | OR
  | SLT
  | SLTU
  | MFHI
  | MFLO
  | MULT
  | NOP
  | SLL
  | SRL
  | SRA
  | SUB
  | SUBU
  | SYSCALL
  | XOR
  -- I-type
  | ADDI
  | ADDIU
  | ANDI
  | BEQ
  | BGEZ
  | BLEZ
  | BNE
  | LB
  | LBU
  | LHU
  | LUI
  | LW
  | LWC1
  | LDC1
  | ORI
  | SLTI
  | SLTIU
  | SB
  | SH
  | SW
  | SWC1
  -- J-type
  | J
  | JAL
  -- FR-type
  | ADDS
  | ADDD
  | CVTDS
  | CVTDW
  | CVTSW
  | CVTSD
  | DIVD
  | DIVS
  | MOVS
  | MOVD
  | MFC1
  | MTC1
  | MULS
  | MULD
  deriving (Show)

type Shamt = Word8

data InstrWord where
  RInstr  :: Instr -> RegName -> RegName -> RegName -> Shamt -> InstrWord
  IInstr  :: Instr -> RegName -> RegName -> Int16            -> InstrWord
  JInstr  :: Instr -> Word32                                 -> InstrWord
  FRInstr :: Instr -> FPRegName -> FPRegName -> FPRegName    -> InstrWord
  Syscall ::                                                    InstrWord

class Show a => PShow a where
  pshow :: a -> String
  pshow v = show v

instance PShow RegName where
  pshow r = "$" <> show r
instance PShow FPRegName where
  pshow r = "$" <> show r
instance PShow Instr
instance PShow Word8
instance PShow Int16
instance PShow String where
  pshow s = s

data ShowBox = forall s. PShow s => Bx s
instance Show ShowBox where
  show (Bx s) = pshow s

showBox :: InstrWord -> [ShowBox]
showBox (RInstr i rs rt rd sa) =
  case i of
    BREAK -> [Bx BREAK]
    JR    -> [Bx JR,  Bx rs]
    JALR  -> if rd == Ra
      then   [Bx JALR, Bx rs]
      else   [Bx JALR, Bx rd, Bx rs]
    MFHI  -> [Bx MFHI, Bx rd]
    MFLO  -> [Bx MFLO, Bx rd]
    MULT  -> [Bx MULT, Bx rs, Bx rt]
    SLL   -> if (Zero, Zero, Zero, 0) == (rs, rt, rd, sa)
      then   [Bx NOP]
      else   [Bx SLL, Bx rd, Bx rt, Bx sa]
    SRA   -> [Bx SRA, Bx rd, Bx rt, Bx sa]
    SRL   -> [Bx SRL, Bx rd, Bx rt, Bx sa]
    _     -> [Bx i,   Bx rd, Bx rs, Bx rt]
showBox (IInstr i rs rt w) =
  case i of
    LB   -> [Bx LB, Bx rt, Bx rswhex]
    LBU  -> [Bx LBU, Bx rt, Bx rswhex]
    LUI  -> [Bx LUI, Bx rt, Bx w]
    LW   -> [Bx LW, Bx rt, Bx rswhex]
    LWC1 -> [Bx LWC1, Bx rtfp, Bx rswhex]
    LDC1 -> [Bx LDC1, Bx rtfp, Bx rswhex]
    BEQ  -> [Bx BEQ, Bx rs, Bx rt, Bx w]
    BGEZ -> [Bx BGEZ, Bx rs, Bx w]
    BLEZ -> [Bx BLEZ, Bx rs, Bx w]
    BNE  -> [Bx BNE, Bx rs, Bx rt, Bx w]
    SB   -> [Bx SB,  Bx rs, Bx rt, Bx w]
    SH   -> [Bx SH,  Bx rs, Bx rt, Bx w]
    SW   -> [Bx SW, Bx rt, Bx rswhex]
    SWC1 -> [Bx SWC1, Bx rtfp, Bx rswhex]
    _    -> [Bx i,   Bx rt, Bx rs, Bx w]
  where
    rtfp   = w2fpreg (fromIntegral $ fromEnum rt)
    rswhex = "0x" <> showHex w "(" <> pshow rs <>")"
showBox (JInstr i w) = [Bx i, Bx $ "0x" <> showHex w " # 0x" <> showHex (w `shiftL` 2) ""]
showBox (FRInstr i ft fs fd) =
  case i of
    ADDS  -> [Bx "add.s", Bx fd, Bx fs, Bx ft]
    ADDD  -> [Bx "add.d", Bx fd, Bx fs, Bx ft]
    CVTDS -> [Bx "cvt.d.s", Bx fd, Bx fs]
    CVTDW -> [Bx "cvt.d.w", Bx fd, Bx fs]
    CVTSW -> [Bx "cvt.s.w", Bx fd, Bx fs]
    CVTSD -> [Bx "cvt.s.d", Bx fd, Bx fs]
    DIVD  -> [Bx "div.d", Bx fd, Bx fs, Bx ft]
    DIVS  -> [Bx "div.s", Bx fd, Bx fs, Bx ft]
    MFC1  -> [Bx MFC1, Bx (toEnum $ fromEnum ft :: RegName), Bx fs]
    MOVS  -> [Bx "mov.s", Bx fd, Bx fs]
    MOVD  -> [Bx "mov.d", Bx fd, Bx fs]
    MTC1  -> [Bx MTC1, Bx (toEnum $ fromEnum ft :: RegName), Bx fs]
    MULS  -> [Bx "mul.s", Bx fd, Bx fs, Bx ft]
    MULD  -> [Bx "mul.d", Bx fd, Bx fs, Bx ft]
    _ -> error $ "Operação de disassembly para instrução não definida: " ++ show i
showBox Syscall =  [Bx "syscall"]

instance Show InstrWord where
  show iw = map toLower $
    ins <> " " <> intercalate ", " ops
    where
      (ins:ops) = show <$> showBox iw
