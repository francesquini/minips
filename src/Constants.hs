{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE KindSignatures #-}
module Constants where

import Data.Word

traceFile :: FilePath
traceFile = "minips.trace"

-- Section locations
textAddress, roDataAddress, dataAddress :: Num a => a
textAddress   = 0x00400000
roDataAddress = 0x00800000
dataAddress   = 0x10010000

-- Initial register values
spAddress, gpAddress, pcAddress :: Word32
spAddress     = 0x7fffeffc
gpAddress     = 0x10008000
pcAddress     = textAddress

data Unity = Byte | W32 | Kibi | Mebi

class ByteSize (a :: Unity) where
  multiplier  :: Int

  maxTextSize :: Int
  maxTextSize = (roDataAddress - textAddress) `div` multiplier @a

  maxRodataSize :: Int
  maxRodataSize = (dataAddress - roDataAddress) `div` multiplier @a

instance ByteSize Byte where
  multiplier = 1

instance ByteSize W32 where
  multiplier = 4

instance ByteSize Kibi where
  multiplier = 1024

instance ByteSize Mebi where
  multiplier = multiplier @Kibi * multiplier @Kibi

data SIMultiplier = None | Kilo | Mega

class SIMultiplierC (a :: SIMultiplier) where
  siMultiplier :: Num b => b

instance SIMultiplierC None where
  siMultiplier = 1

instance SIMultiplierC Kilo where
  siMultiplier = 1000

instance SIMultiplierC Mega where
  siMultiplier = 1000000

data FrequencyUnit = Hertz | KiloHertz | MegaHertz
class Frequency (a :: FrequencyUnit) where
  factor :: Int
  -- Picoseconds -> ?Hz
  frequency :: Double -> Double
  frequency x = (1 / (x * (10**(-12)))) / fromIntegral (factor @a)
instance Frequency KiloHertz where
  factor = siMultiplier @Kilo
instance Frequency MegaHertz where
  factor = siMultiplier @Mega

-- Picos
monocycleClockPeriod :: Double
monocycleClockPeriod = multicycleClockPeriod * 4

-- Picos
multicycleClockPeriod :: Double
multicycleClockPeriod = 29525.6991686

monocycleFrequency :: Double
monocycleFrequency = frequency @MegaHertz monocycleClockPeriod

multicycleFrequency :: Double
multicycleFrequency = frequency @MegaHertz multicycleClockPeriod

picoToSeconds :: Double -> Double
picoToSeconds p = p / (10**12)
