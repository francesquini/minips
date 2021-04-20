{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MemoryElement (
    Latency
  , LineIx
  , MemoryLine
  , MemoryLevel (..)
  , MemoryAccessResponse
  , Log
  , MemoryLevelST
  , runMemoryLevelST
  , mkRAM
  , mkCache
  , memoryLevelRecursiveFlush
  , AccessStats(..)
  , resetMLStats
  , getMLStats
  , randomReplacementPolicy
  , lruReplacementPolicy
  , readLineLevel
  , writeLineLevel
  , invalidateLineLevel
  , nextLevel) where

import Prelude hiding (read)

import Data.Bits
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Vector (Vector)
import qualified Data.Vector as V hiding ((//), (!))
import Data.Word
import GHC.Base (assert)
import Utils
import Data.Bifunctor
import System.Random
import Control.Monad.State.Strict
import Data.Functor ((<&>))
import Control.Monad.Writer.Strict
import Data.Maybe

type LineIx      = Int
type Latency     = Int -- cycles
type MemoryLine  = Vector Word32
data AccessStats = AccessStats {
    _hits   :: !Int
  , _misses :: !Int}
type SetIndex    = Int
type Tag         = Int

data CacheEntry  = CacheEntry {
    ceValid      :: !Bool
  , ceDirty      :: !Bool
  , _ceTag       :: !Tag
  , ceLineIx     :: !LineIx -- Até daria para ser calculado, mas economiza tempo deixar guardado
  , ceMemLine    :: !MemoryLine
  , ceLastAccess :: Int
  } deriving Show

emptyCE :: Int -> CacheEntry
emptyCE lnSz =
  CacheEntry False False 0 0 (V.generate wordsPerLine $ const 0) 0
  where
    wordsPerLine = lnSz `div` 4

ceNeedsWriteBack :: CacheEntry -> Bool
ceNeedsWriteBack (CacheEntry v d _ _ _ _) = v && d

ceMatches :: Tag -> CacheEntry -> Bool
ceMatches tag0 (CacheEntry vl _ tag1 _ _ _) = vl && tag0 == tag1

-- -- set way/cacheEntry
newtype Set = Set (Vector CacheEntry) deriving (Semigroup, Monoid)

type ReplacementPolicy = SetIndex -> MemoryLevelST Int -- set index -> way number
type ReplacementPolicyFactory = MemoryLevel -> ReplacementPolicy

data MemoryLevel =
    Cache {
        description        :: String
      , latency            :: Latency -- in cycles
      , cSize              :: Int -- in bytes
      , cLineSize          :: Int -- in bytes
      , cAssociativity     :: Int
      , cSets              :: Vector Set
      , cSetCount          :: Int
      , cIndexAndTag       :: LineIx -> (Int, Int)
      , cReplacementPolicy :: ReplacementPolicy
      , cNextLevel         :: MemoryLevel
      , stats              :: !AccessStats
      , tracingEnabled     :: Bool
      , ticks              :: !Int -- Ticks are used as a timestamp on cache lines for lru
    }
  | RAM {
        description    :: String
      , rMem           :: IntMap MemoryLine
      , latency        :: Latency
      , stats          :: !AccessStats
      , rZeros         :: Vector Word32
      , tracingEnabled :: Bool
      , ticks          :: !Int -- unused, but here to have the same interface as caches
      }

type Log = [String]
type MemoryAccessResponse a = (a, Log)
type MemoryLevelST = StateT MemoryLevel (Writer Log)

runMemoryLevelST :: MemoryLevelST a -> MemoryLevel -> MemoryAccessResponse (a, MemoryLevel)
runMemoryLevelST f s =  runWriter $ runStateT f s

traceMLST :: Log -> MemoryLevelST ()
traceMLST strs = whenM (gets tracingEnabled) $ tell strs

cacheNextLevelRun :: MemoryLevelST a -> MemoryLevelST a
cacheNextLevelRun f = do
  s <- get
  let ((ret, nxtLevel), lg) = runMemoryLevelST f (cNextLevel s)
  put s{cNextLevel=nxtLevel}
  traceMLST $ mappend "\t" <$> lg
  return ret

logMemActivity :: String -> MemoryLevelST ()
logMemActivity act = do
  ml <- get
  traceMLST [description ml <> ": " <> act]

randomReplacementPolicy :: RandomGen g => g -> ReplacementPolicyFactory
randomReplacementPolicy rg ~ca@Cache{} =
  randomReplacementPolicy0 rList
  where
    rList = randomRs (0, cAssociativity ca - 1) rg

randomReplacementPolicy0 :: [Int] -> ReplacementPolicy
randomReplacementPolicy0 ~(i : is) _setIndex = do
  logMemActivity $ "Random replacement policy. Way#" <> show i
  state (\ca -> (i, ca{cReplacementPolicy = randomReplacementPolicy0 is}))

lruReplacementPolicy :: ReplacementPolicyFactory
lruReplacementPolicy _ = lruReplacementPolicy0
  where
    lruReplacementPolicy0 :: ReplacementPolicy
    lruReplacementPolicy0 setIndex = do
      Set st <- gets cSets <&> (`V.unsafeIndex` setIndex)
      -- Set st <- gets cSets <&> (V.! setIndex)
      let ix = V.minIndexBy (\a b -> compare (ceLastAccess a) (ceLastAccess b)) st
      logMemActivity $ "LRU replacement policy. Way#" <> show ix
      return ix

mkCache :: String -> Latency -> Int -> Int -> Int -> MemoryLevel -> ReplacementPolicyFactory -> Bool -> MemoryLevel
mkCache descr lat sz lnSz assoc nxt replacementPol traceEnabled =
  assert (powerOf2 sz) $
  assert (powerOf2 lnSz && lnSz < sz) $
  assert (powerOf2 setCnt) $
  let ret = Cache
              descr
              lat
              sz
              lnSz
              assoc
              setsV
              setCnt
              (indexAndTag0 ret)
              (replacementPol ret)
              nxt
              (AccessStats 0 0)
              traceEnabled
              0
  in ret
  where
    setCnt = sz `div` (assoc * lnSz)
    mkSet = Set $ V.generate assoc $ const (emptyCE lnSz)
    setsV = V.generate setCnt $ const mkSet


indexAndTag0 :: MemoryLevel -> LineIx -> (SetIndex, Tag)
indexAndTag0 ~ca@Cache{} lineIx =
  (index, tag)
  where
    indexMax = cSetCount ca
    index    = lineIx .&. (indexMax - 1)
    tag      = lineIx `shiftR` countTrailingZeros indexMax

mkRAM :: String -> Int -> Latency -> Bool -> MemoryLevel
mkRAM desc lineSz ltcy traceEnabled = assert (powerOf2 lineSz) $
  RAM
    desc
    IM.empty
    ltcy
    (AccessStats 0 0)
    (V.replicate (lineSz `div` 4) 0)
    traceEnabled
    0

scoreStat :: String -> (AccessStats -> AccessStats) -> MemoryLevelST ()
scoreStat lg f = do
  logMemActivity lg
  modify $ \s -> s{stats = f (stats s)}

scoreHit :: MemoryLevelST ()
scoreHit = scoreStat "Hit" hit
  where
    hit (AccessStats h m) = AccessStats (h + 1) m

scoreMiss :: MemoryLevelST ()
scoreMiss = scoreStat "Miss" miss
  where
    miss (AccessStats h m) = AccessStats h (m + 1)

cacheTick :: MemoryLevelST ()
cacheTick = modify $ \s -> s{ticks= ticks s + 1}

updateCacheEntryAndTick :: SetIndex -> Int -> (CacheEntry -> CacheEntry) -> Bool -> MemoryLevelST ()
updateCacheEntryAndTick setIndex wayNb updFun shouldTick= do
  tick <- gets ticks
  sets <- gets cSets
  let Set st  = sets `V.unsafeIndex` setIndex
      ce      = updFun $ st `V.unsafeIndex` wayNb
      newSet  = Set $ st `V.unsafeUpd`
        [(wayNb, if shouldTick then ce{ceLastAccess=tick} else ce)]
      newSets = sets `V.unsafeUpd` [(setIndex, newSet)]
  modify $ \s -> s{cSets=newSets}

touchCacheEntry :: SetIndex -> Int -> MemoryLevelST ()
touchCacheEntry setIndex wayNb = updateCacheEntryAndTick setIndex wayNb id True

memoryLevelRecursiveFlush :: MemoryLevelST Latency
memoryLevelRecursiveFlush = memoryLevelFlush True

memoryLevelFlush :: Bool -> MemoryLevelST Latency
memoryLevelFlush recursive = do
  nxtLvl <- gets nextLevel
  case nxtLvl of
    Just _ -> do
      lat0 <- memoryLevelFlush'
      lat1 <- if recursive then
                cacheNextLevelRun $ memoryLevelFlush True
              else
                return 0
      return $ lat0 + lat1
    Nothing -> return 0
  where
    flushCE :: Int -> CacheEntry -> MemoryLevelST (Latency, CacheEntry)
    flushCE tick ce
      | ceNeedsWriteBack ce = do
          lat <- cacheNextLevelRun $ writeLineLevel (ceLineIx ce) (const $ ceMemLine ce)
          return (lat, flushedCE)
      | otherwise = return (0, flushedCE)
      where
        --set valid to false is enough, but hey, why not mark as clean?
        flushedCE = ce{ceValid=False, ceDirty=False, ceLastAccess=tick}

    flushSet :: Int -> Set -> MemoryLevelST (Sum Latency, Set)
    flushSet tick (Set st) = foldMap (bimap Sum (Set . V.singleton)) <$> V.mapM (flushCE tick) st

-- flush sets, then each way
    memoryLevelFlush' :: MemoryLevelST Latency
    memoryLevelFlush' = do
      tick <- gets ticks
      newLatAndSets <- gets cSets >>= V.mapM (flushSet tick)
      modify $ \s -> s{cSets = V.map snd newLatAndSets}
      return $ getSum (foldMap fst newLatAndSets)

resetMLStats :: MemoryLevelST ()
resetMLStats = do
  whenM (gets $ isJust . nextLevel) $ do
    cacheNextLevelRun resetMLStats
  modify $ \s -> s{stats=AccessStats 0 0}

getMLStats :: MemoryLevelST (String, AccessStats)
getMLStats = do
  gets $ \ml -> (description ml, stats ml)

-- -> (way#, mem line)
cacheSearch :: (SetIndex, Tag) -> MemoryLevel -> Maybe (Int, MemoryLine)
cacheSearch (setIndex, tag) ~ca@Cache{} =
      (\i -> (i, ceMemLine $ st `V.unsafeIndex` i)) <$> wayNumber
      where
        (Set st) = cSets ca `V.unsafeIndex` setIndex
        wayNumber = V.findIndex (ceMatches tag) st

cacheUpdateLine :: LineIx -> (SetIndex, Tag) -> Int -> MemoryLine -> MemoryLevelST Latency
cacheUpdateLine lineIx (setIndex, tag) wayNb memLine = do
  logMemActivity $ "updating line# 0x" <> showHex32 (fromIntegral lineIx)
  updateCacheEntryAndTick
    setIndex
    wayNb
    (const $ CacheEntry True True tag lineIx memLine undefined)
    True
  gets latency

cacheReplaceLine :: LineIx -> (SetIndex, Tag) -> MemoryLine -> Bool -> MemoryLevelST Latency
cacheReplaceLine lineIx (setIndex, newTag) memLine dirty = do
  logMemActivity $ "Replace to include line# 0x" <> showHex32 (fromIntegral lineIx)
  wayNb  <- gets cReplacementPolicy >>$ setIndex

  Set oldSet <- gets cSets <&> (`V.unsafeIndex` setIndex)
  let oldCe = oldSet `V.unsafeIndex` wayNb

  lat0 <- if ceNeedsWriteBack oldCe then do
      logMemActivity $ "Writing back line: 0x" <> showHex32 (fromIntegral $ ceLineIx oldCe)
      cacheNextLevelRun $ writeLineLevel (ceLineIx oldCe) (const $ ceMemLine oldCe)
    else do
      logMemActivity "Line clean/invalid. No need to write back."
      return 0
  updateCacheEntryAndTick
    setIndex
    wayNb (const $ CacheEntry True dirty newTag lineIx memLine undefined)
    True
  gets $ (+ lat0) . latency

-- Line to be read, sister mem. level, recurring call (if recurring does not log/count accesses)
readLineLevel :: LineIx -> Maybe MemoryLevel -> Bool -> MemoryLevelST (MemoryLine, Latency)
readLineLevel lnIx mml recur = do
  unless recur $ do
    cacheTick
    logMemActivity $ "read line# 0x" <> showHex32 (fromIntegral lnIx)
  ml <- get
  case ml of
    RAM{}   -> readLineLevelRAM ml
    Cache{} -> readLineLevelCache
  where

    readLineLevelRAM :: MemoryLevel -> MemoryLevelST (MemoryLine, Latency)
    readLineLevelRAM ml = do
      unless recur scoreHit -- Always a hit in RAM
      let mLine = IM.findWithDefault (rZeros ml) lnIx (rMem ml)
      gets $ \s -> (mLine, latency s)

    readLineLevelCache :: MemoryLevelST (MemoryLine, Latency)
    readLineLevelCache =  do
      indexAndTag <- gets (`cIndexAndTag` lnIx)
      maybeMLine <- gets $ cacheSearch indexAndTag
      maybe
        (readLineLevelCacheMiss indexAndTag) -- Cache Miss
        (readLineLevelCacheHit indexAndTag)  -- Cache Hit
        maybeMLine

    readLineLevelCacheHit :: (SetIndex, Tag) -> (Int, MemoryLine) -> MemoryLevelST (MemoryLine, Latency)
    readLineLevelCacheHit indexAndTag (mway, mLine) = do
      scoreHit
      touchCacheEntry (fst indexAndTag) mway
      gets $ \s -> (mLine, latency s)

    readLineLevelCacheMiss :: (SetIndex, Tag) -> MemoryLevelST (MemoryLine, Latency)
    readLineLevelCacheMiss indexAndTag = do
      unless recur scoreMiss -- it is a miss..
      -- now, if it's an I-cache needs to search on sister
      case mml of
        Nothing -> do -- No sister, perform a regular read next level
          -- not recurring since it is on the next level
          (mLine, lat0) <- cacheNextLevelRun $ readLineLevel lnIx Nothing False
          lat1 <- cacheReplaceLine lnIx indexAndTag mLine False
          return (mLine, lat0 + lat1)
        Just sister -> do
          let sisterIndexAndTag = cIndexAndTag sister lnIx
              mSisterLine = cacheSearch sisterIndexAndTag sister
          case mSisterLine of
            Nothing -> -- then, it is not on the sister. Perform a regular Access
              readLineLevel lnIx Nothing True
            (Just (_, mline)) -> do -- sister has it! replace my line and continue
              -- notice that sister cache does not get an update in access times!!
              lat <- cacheReplaceLine lnIx sisterIndexAndTag mline False
              return (mline, lat)

writeLineLevel :: LineIx -> (MemoryLine -> MemoryLine) -> MemoryLevelST Latency
writeLineLevel lnIx updateLine = do
  cacheTick
  logMemActivity $ "write: 0x" <> showHex32 (fromIntegral lnIx)
  ml <- get
  case ml of
    RAM{} -> do
      scoreHit
      let memline0 = IM.findWithDefault (rZeros ml) lnIx (rMem ml)
          memline  = updateLine memline0
      newMem <- gets $ IM.insert lnIx memline . rMem
      state $ \s -> (latency s, s{rMem = newMem})
    Cache{} -> do
      indexAndTag <- gets (`cIndexAndTag` lnIx)
      mbeline <- gets $ cacheSearch indexAndTag
      case mbeline of
        Nothing -> do -- Cache miss on write
          scoreMiss
          -- perform a read on the next level
          -- not recurring since it is on the next level
          (memline0, lat0) <- cacheNextLevelRun $ readLineLevel lnIx Nothing False
          let memline = updateLine memline0
          lat1 <- cacheReplaceLine lnIx indexAndTag memline True
          return $ lat0 + lat1
        Just (wayn, memline0) -> do -- Cache hit on write
          scoreHit
          let memline = updateLine memline0
          cacheUpdateLine lnIx indexAndTag wayn memline


invalidateLineLevel :: LineIx -> MemoryLevelST Bool -- if line was found
invalidateLineLevel lnIx = do
  ml <- get
  case ml of
    Cache{} -> do
      let indexAndTag = cIndexAndTag ml lnIx
          mmline = cacheSearch indexAndTag ml
      when (isJust mmline) $ do
        let wayn = fst $ fromJust mmline
        updateCacheEntryAndTick
          (fst indexAndTag)
          wayn
          (\ce -> assert (not $ ceDirty ce) ce{ceValid=False})
          False
        logMemActivity $ "invalidating line 0x" <> showHex32 (fromIntegral lnIx)
      return $ isJust mmline
    RAM {}  -> return False

nextLevel :: MemoryLevel ->  Maybe MemoryLevel
nextLevel Cache{cNextLevel=l} = Just l
nextLevel RAM{} = Nothing
