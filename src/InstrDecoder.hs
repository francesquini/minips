module InstrDecoder (disassemble, decodeInstruction) where

import Architecture
import Constants
import Utils
import Data.Bits
import Data.Int
import Data.Word

disassemble :: Executable -> String
disassemble (txt, _, _) = unlines $
  zipWith3 (\a b c -> a ++ ": \t" ++  b ++ "\t" ++ c) addrs ws' insts
  where
    insts = map (show . decodeInstruction) txt
    ws'   = map showHex32 txt
    addrs = map showHex32 [textAddress, textAddress + 4 ..]

decodeInstruction :: Word32 -> InstrWord
decodeInstruction w =
  case getOpcode w of
    -- R-Type
    0x00 -> decodeRType w
    -- J-type
    0x02 -> decodeJType w
    0x03 -> decodeJType w
    -- FR-type
    0x11 -> decodeFType w
    -- I-type
    _    -> decodeIType w


decodeFType :: Word32 -> InstrWord
decodeFType w =
  case fmt of
    0x08 ->
      FIInstr (decodeFIType fmt ft im)
        (w2fpreg ft)
        im
    _    ->
      FRInstr
        (decodeFRType fmt ft fs fd funct)
        (w2fpreg ft)
        (w2fpreg fs)
        (w2fpreg fd)
  where
    fmt   = getFmt w
    ft    = getFt w
    fs    = getFs w
    fd    = getFd w
    funct = getFunct w
    im    = getImmediate w

decodeFIType ::  Word8 ->  Word8 -> Int16 -> Instr
decodeFIType _fmt ft _imm =
  case ft of
    0x00 -> BC1F
    0x01 -> BC1T
    _ -> error "Instrução tipo FI não reconhecida 1."

decodeFRType ::  Word8 ->  Word8 -> Word8 -> Word8 -> Word8 -> Instr
decodeFRType fmt ft _ fd funct
  | funct == 0x00 =
    case fmt of
      0x10 -> ADDS
      0x11 -> ADDD
      _    -> if fd == 0x00 then
                case fmt of
                  0x00 -> MFC1
                  0x04 -> MTC1
                  _    -> error "Instrução tipo FR não reconhecida 1."
              else
                error $ "Instrução tipo FR não reconhecida 2. (" ++ show fmt ++ ")"
  | funct == 0x01 = case fmt of
      0x10 -> SUBS
      _    -> error $ "Instrução tipo FR não reconhecida 6. Fmt: 0x" ++ showHex32 (fromIntegral fmt)
  | funct == 0x02 =
    case fmt of
      0x10 -> MULS
      0x11 -> MULD
      _    -> error "Instrução tipo FR não reconhecida 3."
  | funct == 0x03 =
    case fmt of
      0x10 -> DIVS
      0x11 -> DIVD
      _    -> error "Instrução tipo FR não reconhecida 4."

  | funct == 0x06 =
    case fmt of
        0x10 -> MOVS
        0x11 -> MOVD
        _    -> error "Instrução tipo FR não reconhecida 5."
  | funct == 0x20 =
    if ft == 0x00 then
      case fmt of
        0x11 -> CVTSD
        0x14 -> CVTSW
        _    -> error $ "Instrução tipo FR não reconhecida 7. Fmt: 0x" ++ showHex32 (fromIntegral fmt)
    else
      error "Instrução tipo FR não reconhecida 8."
  | funct == 0x21 = case fmt of
        0x10 -> CVTDS
        0x14 -> CVTDW
        _    -> error $ "Instrução tipo FR não reconhecida 9. (" ++ show fmt ++ ")"
  | funct == 0x3c = case fmt of
        0x10 -> CLTS
        _    -> error "Instrução tipo FR não reconhecida 10."
  | otherwise = error $ "Instrução tipo FR não reconhecida 11." ++
    " Funct: 0x" ++ showHex32 (fromIntegral funct) ++
    " Fmt: 0x" ++ showHex32 (fromIntegral fmt)


decodeRType :: Word32 -> InstrWord
decodeRType w =
  if funct == 0x0c
    then Syscall
    else decodeRType' w funct
  where
    funct = getFunct w

decodeRType' :: Word32 -> Word8 -> InstrWord
decodeRType' w funct =
  RInstr instr (getRsR w) (getRtR w) (getRdR w) (getShamt w)
  where
    instr = case funct of
      0x20 -> ADD
      0x21 -> ADDU
      0x24 -> AND
      0x0d -> BREAK
      0x1a -> DIV
      0x1b -> DIVU
      0x09 -> JALR
      0x08 -> JR
      0x12 -> MFLO
      0x10 -> MFHI
      0x18 -> MULT
      0x27 -> NOR
      0x25 -> OR
      0x2a -> SLT
      0x2b -> SLTU
      0x00 -> SLL
      0x03 -> SRA
      0x02 -> SRL
      0x22 -> SUB
      0x23 -> SUBU
      0x26 -> XOR
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
    rt = getRt w
    instr = case op of
      0x08 -> ADDI
      0x09 -> ADDIU
      0x0c -> ANDI
      0x04 -> BEQ
      0x01 -> if rt == 0x01 then BGEZ else undefined
      0x06 -> if rt == 0x00 then BLEZ else undefined
      0x05 -> BNE
      0x20 -> LB
      0x24 -> LBU
      0x21 -> LH
      0x25 -> LHU
      0x0f -> LUI
      0x23 -> LW
      0x31 -> LWC1
      0x35 -> LDC1
      0x0d -> ORI
      0x0a -> SLTI
      0x0b -> SLTIU
      0x28 -> SB
      0x29 -> SH
      0x2b -> SW
      0x39 -> SWC1
      _    ->
        error $ "Instrução tipo I não reconhecida. Opcode: " ++ show op ++ " Word: 0x" ++ showHex w ""

getOpcode, getRs, getRt, getRd, getShamt, getFunct :: Word32 -> Word8
getOpcode    w = fromIntegral $ (w .&. 0xfc000000) `shiftR` 26
getRs        w = fromIntegral $ (w .&. 0x03e00000) `shiftR` 21
getRt        w = fromIntegral $ (w .&. 0x001f0000) `shiftR` 16
getRd        w = fromIntegral $ (w .&. 0x0000f800) `shiftR` 11
getShamt     w = fromIntegral $ (w .&. 0x000007c0) `shiftR` 6
getFunct     w = fromIntegral $  w .&. 0x0000003f

getFmt, getFt, getFs, getFd :: Word32 -> Word8
getFmt         = getRs
getFt          = getRt
getFs          = getRd
getFd          = getShamt

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
