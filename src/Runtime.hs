module Runtime where

import Architecture
import Utils

import Control.Exception

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Bits
import Data.Char
import Data.Function ((&))
import Data.Word

data Minips = Minips {
    endianess  :: Endianness
  , memory     :: IntMap Word32
  , registers  :: IntMap Word32
  , rTypeICount :: Int
  , iTypeICount :: Int
  , jTypeICount :: Int
  }

makeMinips :: Endianness -> [Word32] -> [Word32] -> Minips
makeMinips end txt dt =
  Minips end mem regs 0 0 0
  where
    textAddr  = 0x00400000
    dataAddr  = 0x10010000
    spAddr    = 0x7fffeffc
    gpAddr    = 0x10008000
    pcAddr    = fromIntegral textAddr
    mem    = IM.fromList $
                 zip [textAddr, textAddr + 4 ..] txt ++
                 zip [dataAddr, dataAddr + 4 ..] dt
    updateReg r v = IM.update (const $ Just v) (fromEnum r)
    regs =
         IM.fromList (zip (fromEnum <$> [Zero ..]) (repeat 0))
      & updateReg Sp spAddr
      & updateReg Gp gpAddr
      & updateReg Pc pcAddr

prettyPrint  :: Minips -> String
prettyPrint  (Minips _ mem regs _ _ _) =
  unlines (map pp (IM.assocs regs)) ++
  "\n-------------------------------\n" ++
  unlines (map pp2 (IM.assocs mem))
  where
    pp (reg, val) = unwords [show (toEnum reg :: RegName), showHex val ""]
    pp2 (ad, val) = unwords [showHex ad ":", showHex val ""]

regVal :: RegName -> Minips -> Word32
regVal regName st =
  fromIntegral $ registers st IM.! fromEnum regName

memVal :: Word32 -> Minips -> Word32
memVal ad st = assert (ad `mod` 4 == 0) $
  fromIntegral $ IM.findWithDefault 0 (fromIntegral ad) (memory st)

-- Accepts unaligned addresses
readString :: Word32 -> Minips -> String
readString memAddr st@(Minips e _ _ _ _ _) =
  -- trace ("Lendo string no:\n\tend 0x" <> showHex memAddr "" <>
  --        "\n\talinhado: 0x" <> showHex alignedAddr " com offset " <> show offset <>
  --        "\n\tcom valor 0x" <> showHex (memVal alignedAddr st) "") $
  chr <$> takeWhile (/= 0) mvals
  where
    alignedAddr = memAddr .&. 0xfffffffc
    offset      = fromIntegral $ memAddr .&. 0x00000003
    fixEnd = if e == Big then id else reverse
    mvals  = drop offset $ concatMap (fixEnd . breakWord . (`memVal` st)) [alignedAddr, alignedAddr + 4 ..]

getCounts :: Minips -> (Int, Int, Int)
getCounts (Minips _ _ _ r i j) = (r, i, j)

-- Write OPs
updateRegister :: Integral32 a => RegName -> a -> Minips -> Minips
updateRegister Zero _ m = m
updateRegister r v m@Minips {registers=regs} =
  m{registers= regs'}
  where
    regs' = IM.insert (fromEnum r) (fromIntegral v)  regs

updateMemory :: Word32 -> Word32 -> Minips -> Minips
updateMemory addr v m@Minips{memory = mem} =  assert (addr `mod` 4 == 0) $
  m{memory = mem'}
  where
    mem' = IM.insert (fromIntegral addr) v mem

incPC :: Minips -> Minips
incPC st = updateRegister Pc (regVal Pc st + 4) st

incRICount, incIICount, incJICount :: Minips -> Minips
incRICount m@Minips{rTypeICount=c} = m{rTypeICount = c + 1}
incIICount m@Minips{iTypeICount=c} = m{iTypeICount = c + 1}
incJICount m@Minips{jTypeICount=c} = m{jTypeICount = c + 1}
