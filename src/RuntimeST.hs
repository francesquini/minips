module RuntimeST (
      module RuntimeST
    , R.prettyPrint
    , R.makeMinips
)where

import Architecture
import Runtime as R
import Utils

import Data.Word

import Control.Monad.State.Strict

type MinipsSt = State Minips

regVal :: RegName -> MinipsSt Word32
regVal regName = gets $ R.regVal regName

memVal :: Word32 -> MinipsSt Word32
memVal ad = gets $ R.memVal ad

infixr 4 !<
(!<) :: Integral32 a => RegName -> a -> MinipsSt ()
(!<) r v = modify $ R.updateRegister r v

incPC :: MinipsSt ()
incPC = modify R.incPC
