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

type MinipsST = StateT R.Minips IO

regRead :: RegName -> MinipsST Word32
regRead = gets . R.regRead

memRead :: Word32 -> MinipsST Word32
memRead = gets . R.memRead

readString :: Word32 -> MinipsST String
readString = gets . R.readString

getCounts :: MinipsST (Int, Int, Int)
getCounts = gets R.getCounts

regWrite :: Integral32 a => RegName -> a -> MinipsST ()
regWrite r = modify . R.regWrite r

infixr 4 !<
(!<) :: Integral32 a => RegName -> a -> MinipsST ()
(!<) = regWrite

memWrite :: Word32 -> Word32 -> MinipsST ()
memWrite addr = modify . R.memWrite addr

decodeInstruction :: Word32 -> MinipsST InstrWord
decodeInstruction = state . R.decodeInstruction

incPC :: MinipsST ()
incPC = modify R.incPC

incRICount, incIICount, incJICount :: MinipsST ()
incRICount = modify R.incRICount
incIICount = modify R.incIICount
incJICount = modify R.incJICount
