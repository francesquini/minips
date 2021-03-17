module RuntimeST (
      module RuntimeST
    , R.prettyPrint
    , R.makeMinips
    , R.ICount
    , R.rTypeICount, R.iTypeICount, R.jTypeICount
    ) where

import Architecture
import qualified Runtime as R
import Utils

import Data.Word as W

import Control.Monad.State.Strict
import Data.Binary.IEEE754

type MinipsST = StateT R.Minips IO

tick :: MinipsST ()
tick = modify R.tick

regRead :: RegName -> MinipsST Word32
regRead = gets . R.regRead

fpRegRead :: FPRegName -> MinipsST Word32
fpRegRead = gets . R.fpRegRead

fpRegReadFloat :: FPRegName -> MinipsST Float
fpRegReadFloat reg = wordToFloat <$> fpRegRead reg

fpRegReadDouble :: FPRegName -> MinipsST Double
fpRegReadDouble reg = do
  a <- fpRegRead reg
  b <- fpRegRead (succ reg)
  return $ doubleFromWords32 b a

memRead :: Word32 -> MinipsST Word32
memRead = gets . R.memRead

readString :: Word32 -> MinipsST String
readString = gets . R.readString

getStats :: MinipsST (Int, R.ICount)
getStats = do
  cycles <- gets R.cycles
  counts <- gets R.iCount
  return (cycles, counts)


regWrite :: Integral32 a => RegName -> a -> MinipsST ()
regWrite r = modify . R.regWrite r

infixr 4 !<
(!<) :: Integral32 a => RegName -> a -> MinipsST ()
(!<) = regWrite

fpRegWrite :: Integral32 a => FPRegName -> a -> MinipsST ()
fpRegWrite r = modify . R.fpRegWrite r

infixr 4 !!<
(!!<) :: Integral32 a => FPRegName -> a -> MinipsST ()
(!!<) = fpRegWrite

fpRegWriteFloat :: FPRegName -> Float -> MinipsST ()
fpRegWriteFloat reg fl = reg !!< floatToWord fl

fpRegWriteDouble :: FPRegName -> Double -> MinipsST ()
fpRegWriteDouble reg db = do
  reg !!< b
  succ reg !!< a
  where
    (a, b) = words32FromDouble db

memWrite :: Word32 -> Word32 -> MinipsST ()
memWrite addr = modify . R.memWrite addr

getBranchDelaySlotAddress :: MinipsST (Maybe Word32)
getBranchDelaySlotAddress = gets R.delaySlotAddr

setBranchDelaySlotAddress :: Maybe Word32 -> MinipsST ()
setBranchDelaySlotAddress = modify . R.setBranchDelaySlotAddress

resetBranchDelaySlotAddress :: MinipsST ()
resetBranchDelaySlotAddress = setBranchDelaySlotAddress Nothing

decodeInstruction :: Word32 -> MinipsST InstrWord
decodeInstruction = state . R.decodeInstruction

incPC :: MinipsST ()
incPC = modify R.incPC

incRICount, incIICount, incJICount, incFRCount :: MinipsST ()
incRICount = modify R.incRICount
incIICount = modify R.incIICount
incJICount = modify R.incJICount
incFRCount = modify R.incFRCount
