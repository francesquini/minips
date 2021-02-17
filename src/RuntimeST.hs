module RuntimeST (
      module RuntimeST
    , R.prettyPrint
    , R.makeMinips
    ) where

import Architecture
import qualified Runtime as R
import Utils

import Data.Word as W

import Control.Monad.State.Strict

type MinipsST = StateT R.Minips IO

regRead :: RegName -> MinipsST Word32
regRead regName = gets $ R.regRead regName

memRead :: Word32 -> MinipsST Word32
memRead ad = gets $ R.memRead ad

readString :: Word32 -> MinipsST String
readString ad = gets $ R.readString ad

getCounts :: MinipsST (Int, Int, Int)
getCounts = gets R.getCounts

infixr 4 !<
(!<) :: Integral32 a => RegName -> a -> MinipsST ()
(!<) r v = modify $ R.regWrite r v

memWrite :: Word32 -> Word32 -> MinipsST ()
memWrite addr value = modify $ R.memWrite addr value

decodeInstruction :: Word32 -> MinipsST InstrWord
decodeInstruction w = state $ R.decodeInstruction w

incPC :: MinipsST ()
incPC = modify R.incPC

incRICount, incIICount, incJICount :: MinipsST ()
incRICount = modify R.incRICount
incIICount = modify R.incIICount
incJICount = modify R.incJICount
