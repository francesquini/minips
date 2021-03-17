module Runtime where

import Architecture
import qualified InstrDecoder as D
import Utils
import Constants

import Control.Exception

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Bits
import Data.Char
import Data.Function ((&))
import Data.Word

type ICount = (Int, Int, Int, Int)
rTypeICount, iTypeICount, jTypeICount, frTypeICount :: ICount -> Int
rTypeICount  = fst4
iTypeICount  = snd4
jTypeICount  = trd4
frTypeICount = fth4

data Minips = Minips {
    endianess     :: Endianness
  , memory        :: IntMap Word32
  , registers     :: IntMap Word32
  , fpRegisters   :: IntMap Word32
  , iCount        :: ICount
  , cycles        :: Int
  , delaySlotAddr :: Maybe Word32
  }

makeMinips :: Endianness -> ([Word32], [Word32], [Word32]) -> Minips
makeMinips end (txt, dt, ro) =
  Minips end mem regs fpregs (0, 0, 0, 0) 0 Nothing
  where
    mem    = IM.fromList $
                 zip [textAddress,   textAddress   + 4 ..] txt ++
                 zip [roDataAddress, roDataAddress + 4 ..] ro  ++
                 zip [dataAddress,   dataAddress   + 4 ..] dt
    updateReg r v = IM.update (const $ Just v) (fromEnum r)
    regs =
         IM.fromList (zip (fromEnum <$> [Zero ..]) (repeat 0))
      & updateReg Sp spAddress
      & updateReg Gp gpAddress
      & updateReg Pc pcAddress
    fpregs = IM.fromList (zip (fromEnum <$> [F0 ..]) (repeat 0))

prettyPrint  :: Minips -> String
prettyPrint  Minips{memory=mem, registers=regs} =
  unlines (map pp (IM.assocs regs)) ++
  "\n-------------------------------\n" ++
  unlines (map pp2 (IM.assocs mem))
  where
    pp (reg, val) = unwords [show (toEnum reg :: RegName), showHex val ""]
    pp2 (ad, val) = unwords [showHex ad ":", showHex val ""]

tick :: Minips -> Minips
tick m@Minips{cycles = c} = m{cycles = c + 1}

regRead :: RegName -> Minips -> Word32
regRead regName st =
  registers st IM.! fromEnum regName

fpRegRead :: FPRegName -> Minips -> Word32
fpRegRead fpRegName st =
  fpRegisters st IM.! fromEnum fpRegName

memRead :: Word32 -> Minips -> Word32
memRead ad st = assert (ad `mod` 4 == 0) $
  IM.findWithDefault 0 (fromIntegral ad) (memory st)

-- Accepts unaligned addresses
-- Reads a string stores in address memAddr
readString :: Word32 -> Minips -> String
readString memAddr st@Minips{endianess=e}=
  chr <$> takeWhile (/= 0) mvals
  where
    alignedAddr = memAddr .&. 0xfffffffc
    offset      = fromIntegral $ memAddr .&. 0x00000003
    fixEnd = if e == Big then id else reverse
    mvals  = drop offset $ concatMap (fixEnd . breakWord . (`memRead` st)) [alignedAddr, alignedAddr + 4 ..]

-- Write OPs
regWrite :: Integral32 a => RegName -> a -> Minips -> Minips
regWrite Zero _ m = m
regWrite r v m@Minips {registers=regs} =
  m{registers= regs'}
  where
    regs' = IM.insert (fromEnum r) (fromIntegral v) regs

fpRegWrite :: Integral32 a => FPRegName -> a -> Minips -> Minips
fpRegWrite r v m@Minips {fpRegisters=fpRegs} =
  m{fpRegisters= fpRegs'}
  where
    fpRegs' = IM.insert (fromEnum r) (fromIntegral v) fpRegs


memWrite :: Word32 -> Word32 -> Minips -> Minips
memWrite addr v m@Minips{memory = mem} =  assert (addr `mod` 4 == 0) $
  m{memory = mem'}
  where
    mem' = IM.insert (fromIntegral addr) v mem

getBranchDelaySlotAddress :: Minips -> Maybe Word32
getBranchDelaySlotAddress = delaySlotAddr

setBranchDelaySlotAddress :: Maybe Word32 -> Minips -> Minips
setBranchDelaySlotAddress mi m = m{delaySlotAddr= mi}

decodeInstruction :: Word32 -> Minips -> (InstrWord, Minips)
decodeInstruction w m = (D.decodeInstruction w, m)

incPC :: Minips -> Minips
incPC st = regWrite Pc (regRead Pc st + 4) st

incRICount, incIICount, incJICount, incFRCount :: Minips -> Minips
incRICount m@Minips{iCount=c} = m{iCount = map1 (+1) c}
incIICount m@Minips{iCount=c} = m{iCount = map2 (+1) c}
incJICount m@Minips{iCount=c} = m{iCount = map3 (+1) c}
incFRCount m@Minips{iCount=c} = m{iCount = map4 (+1) c}
