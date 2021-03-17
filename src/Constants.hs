{-# LANGUAGE DataKinds #-}
module Constants where

import Data.Word

kilo, mega, giga :: Num a => a
kilo = 1000
mega = kilo * 1000
giga = mega * 1000

-- Section locations
textAddress, roDataAddress, dataAddress :: Int
textAddress   = 0x00400000
roDataAddress = 0x00800000
dataAddress   = 0x10010000

-- Initial register values
spAddress, gpAddress, pcAddress :: Word32
spAddress     = 0x7fffeffc
gpAddress     = 0x10008000
pcAddress     = fromIntegral textAddress

-- Picoseconds -> Hz
frequency :: Double -> Double
frequency x = 1 / (x * (10**(-12)))

-- Picos
monocycleClockPeriod :: Double
monocycleClockPeriod = multicycleClockPeriod * 4

-- Picos
multicycleClockPeriod :: Double
multicycleClockPeriod = 29525.6991686

-- In MHz
monocycleFrequency :: Double
monocycleFrequency = frequency monocycleClockPeriod / mega

-- In MHz
multicycleFrequency :: Double
multicycleFrequency = frequency multicycleClockPeriod / mega

picoToSeconds :: Double -> Double
picoToSeconds p = p / (10**12)
