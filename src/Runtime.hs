module Runtime where

import Architecture
import Utils

import Control.Exception

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Word
import Numeric

data Minips = Minips {
    memory    :: IntMap Word32
  , registers :: IntMap Word32
  }

makeMinips :: [Word32] -> [Word32] -> Minips
makeMinips txt dt =
  Minips mem regs
  where
    textAddr  = 0x00400000
    dataAddr  = 0x10000000
    spAddr    = 0x7fffeffc
    gpAddr    = 0x10008000
    pcAddr    = fromIntegral textAddr
    mem    = IM.fromList $
                 zip [textAddr, textAddr + 4 ..] txt ++
                 zip [dataAddr, dataAddr + 4 ..] dt
    updateReg r v = IM.update (const $ Just v) (fromEnum r)
    regs =
         IM.fromList (zip (fromEnum <$> [Zero ..]) (repeat 0))
      |> updateReg Sp spAddr
      |> updateReg Gp gpAddr
      |> updateReg Pc pcAddr

prettyPrint  :: Minips -> String
prettyPrint  (Minips _ regs) =
  unlines $ map pp (IM.assocs regs)
  where
    pp (reg, val) = unwords [show (toEnum reg :: RegName), showHex val ""]

regVal :: RegName -> Minips -> Word32
regVal regName st =
  fromIntegral $ registers st IM.! fromEnum regName

memVal :: Word32 -> Minips -> Word32
memVal ad st =  assert (ad `mod` 4 == 0) $
  fromIntegral $ IM.findWithDefault 0 (fromIntegral ad) (memory st)


-- infixr 4 !<
updateRegister :: Integral32 a => RegName -> a -> Minips -> Minips
updateRegister r v (Minips mem regs) =
  Minips mem regs'
  where
    regs' = IM.update (const $ Just (fromIntegral v)) (fromEnum r) regs

incPC :: Minips -> Minips
incPC st = updateRegister Pc (regVal Pc st + 4) st
