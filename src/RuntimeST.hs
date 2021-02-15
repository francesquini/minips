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

regVal :: RegName -> MinipsST Word32
regVal regName = gets $ R.regVal regName

memVal :: Word32 -> MinipsST Word32
memVal ad = gets $ R.memVal ad
  -- v <- gets $ R.memVal ad
  -- trace (">>>> Read M[0x" <> showHex ad "]= 0x" <> showHex v "")
  -- (return v)

readString :: Word32 -> MinipsST String
readString ad = gets $ R.readString ad

getCounts :: MinipsST (Int, Int, Int)
getCounts = gets R.getCounts

infixr 4 !<
(!<) :: Integral32 a => RegName -> a -> MinipsST ()
(!<) r v =
  -- trace (">>>>" <> show r <> " = 0x" <> showHex v "") $
  modify $ R.updateRegister r v

memWrite :: Word32 -> Word32 -> MinipsST()
memWrite addr value = -- do
  modify $ R.updateMemory addr value
  -- nval <- gets $ R.memVal addr
  -- trace (">>>>W M[0x" <> showHex addr "]= 0x" <> showHex value
  --        " (Read: 0x" <> showHex nval ")")
  --   return()

incPC :: MinipsST ()
incPC = modify R.incPC

incRICount, incIICount, incJICount :: MinipsST ()
incRICount = modify R.incRICount
incIICount = modify R.incIICount
incJICount = modify R.incJICount
