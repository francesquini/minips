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

                -- .txt      .rodata   .data
type Executable = ([Word32], [Word32], [Word32])
data Endianness = Big | Little deriving Eq

type Address = Word32

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

mnemonic :: Instr -> String
mnemonic ADDS  = "add.s"
mnemonic ADDD  = "add.d"
mnemonic CVTDS = "cvt.d.s"
mnemonic CVTDW = "cvt.d.w"
mnemonic CVTSW = "cvt.s.w"
mnemonic CVTSD = "cvt.s.d"
mnemonic DIVD  = "div.d"
mnemonic DIVS  = "div.s"
mnemonic MOVS  = "mov.s"
mnemonic MOVD  = "mov.d"
mnemonic MULS  = "mul.s"
mnemonic MULD  = "mul.d"
mnemonic i     =  map toLower $ show i

type Shamt = Word8

data InstrWord where
  RInstr  :: Instr -> RegName -> RegName -> RegName -> Shamt -> InstrWord
  IInstr  :: Instr -> RegName -> RegName -> Int16            -> InstrWord
  JInstr  :: Instr -> Word32                                 -> InstrWord
  FRInstr :: Instr -> FPRegName -> FPRegName -> FPRegName    -> InstrWord
  Syscall ::                                                    InstrWord

--------------------------
-- Instruction Printing --
--------------------------

class Show a => PShow a where
  basicShow :: a -> String
  basicShow v = show v

  pshow :: a -> String
  pshow v = toLower <$> basicShow v

instance PShow RegName where
  basicShow r = "$" <> show r
instance PShow FPRegName where
  basicShow r = "$" <> show r
instance PShow Instr
instance PShow Word8
instance PShow Int16
instance PShow String where
  basicShow = id

data ShowBox = forall s. PShow s => Bx s
instance Show ShowBox where
  show (Bx s) = pshow s

data ShowIntrType =
  -- Special
    Special
  -- R-Type
  | I
  | IRd
  | IRdRs
  | IRdRsRt
  | IRdRtSa
  -- I-Type
  | IRs
  | IRsRt
  | IRsRtW
  | IRsW
  | IRtfpRsx
  | IRtRsx
  | IRtRsW
  | IRtW
  -- FR-Type
  | IFdFsFt
  | IFdFs
  | IRtFs

rswhex :: PShow a => a -> Int16 -> String
rswhex rs w =
  show w  <> "(" <> pshow rs <>")  # 0x" <> strw
  where
    strw   = showHex32 (fromIntegral (signExtend w :: Int32) :: Word32)

rtfp :: Enum a => a -> FPRegName
rtfp  rt = w2fpreg (fromIntegral $ fromEnum rt)

rtreg :: Enum a => a -> RegName
rtreg ft = toEnum $ fromEnum ft :: RegName

bxi :: Instr -> ShowBox
bxi = Bx . mnemonic

sBoxLst :: InstrWord -> ShowIntrType -> [ShowBox]
sBoxLst (RInstr  i _  _  _  _ ) I        = [bxi i]
sBoxLst (RInstr  i _  _  rd _ ) IRd      = [bxi i, Bx rd]
sBoxLst (RInstr  i rs _  rd _ ) IRdRs    = [bxi i, Bx rd, Bx rs]
sBoxLst (RInstr  i rs rt rd _ ) IRdRsRt  = [bxi i, Bx rd, Bx rs, Bx rt]
sBoxLst (RInstr  i _  rt rd sa) IRdRtSa  = [bxi i, Bx rd, Bx rt, Bx sa]
sBoxLst (RInstr  i rs _  _  _ ) IRs      = [bxi i, Bx rs]
sBoxLst (RInstr  i rs rt _  _ ) IRsRt    = [bxi i, Bx rs, Bx rt]
sBoxLst (IInstr  i rs rt w)     IRsRtW   = [bxi i, Bx rs, Bx rt, Bx w]
sBoxLst (IInstr  i rs _  w)     IRsW     = [bxi i, Bx rs, Bx w]
sBoxLst (IInstr  i rs rt w)     IRtfpRsx = [bxi i, Bx $ rtfp rt, Bx $ rswhex rs w]
sBoxLst (IInstr  i rs rt w)     IRtRsx   = [bxi i, Bx rt, Bx $ rswhex rs w]
sBoxLst (IInstr  i rs rt w)     IRtRsW   = [bxi i, Bx rt, Bx rs, Bx w]
sBoxLst (IInstr  i _  rt w)     IRtW     = [bxi i, Bx rt, Bx w]
sBoxLst (FRInstr i ft fs fd)    IFdFsFt  = [bxi i, Bx fd, Bx fs, Bx ft]
sBoxLst (FRInstr i _  fs fd)    IFdFs    = [bxi i, Bx fd, Bx fs]
sBoxLst (FRInstr i ft fs _ )    IRtFs    = [bxi i, Bx $ rtreg ft, Bx fs]
sBoxLst ins                     Special  =
  case ins of
    (RInstr SLL Zero Zero Zero 0)        -> [Bx "nop"]
    Syscall                              -> [Bx "syscall"]
    _                                    -> undefined
sBoxLst _                       _        =  undefined

showBox :: InstrWord -> [ShowBox]
showBox ins@(RInstr i rs rt rd sa) =
  sBoxLst ins $ case i of
    BREAK -> I
    JR    -> IRs
    JALR  -> if rd == Ra
      then   IRs
      else   IRdRs
    MFHI  -> IRd
    MFLO  -> IRd
    MULT  -> IRsRt
    SLL   -> if (Zero, Zero, Zero, 0) == (rs, rt, rd, sa)
      then   Special
      else   IRdRtSa
    SRA   -> IRdRtSa
    SRL   -> IRdRtSa
    _     -> IRdRsRt
showBox ins@(IInstr i _ _ _) =
  sBoxLst ins $ case i of
    LB   -> IRtRsx
    LBU  -> IRtRsx
    LUI  -> IRtW
    LW   -> IRtRsx
    LWC1 -> IRtfpRsx
    LDC1 -> IRtfpRsx
    BEQ  -> IRsRtW
    BGEZ -> IRsW
    BLEZ -> IRsW
    BNE  -> IRsRtW
    SB   -> IRsRtW
    SH   -> IRsRtW
    SW   -> IRtRsx
    SWC1 -> IRtfpRsx
    _    -> IRtRsW
showBox (JInstr i w) = [bxi i, Bx $ "0x" <> showHex w " # 0x" <> showHex (w `shiftL` 2) ""]
showBox ins@(FRInstr i _ _ _) =
  sBoxLst ins $ case i of
    CVTDS -> IFdFs
    CVTDW -> IFdFs
    CVTSW -> IFdFs
    CVTSD -> IFdFs
    MOVS  -> IFdFs
    MOVD  -> IFdFs
    MFC1  -> IRtFs
    MTC1  -> IRtFs
    _ -> IFdFsFt
showBox Syscall =  sBoxLst Syscall Special

instance Show InstrWord where
  show iw = ins <> " " <> intercalate ", " ops
    where
      (ins:ops) = show <$> showBox iw
