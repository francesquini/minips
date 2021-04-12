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
  , AccessStats
  , resetMLStats
  , randomReplacementPolicy
  , lruReplacementPolicy
  , readLineLevel
  , writeLineLevel
  , nextLevel) where

import Prelude hiding (read)

import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.Base (assert)
import Utils
import Data.Bifunctor
import System.Random
import Control.Monad.State
import Data.Functor ((<&>))
import Control.Monad.Writer
import Data.Maybe

type LineIx      = Int
type Latency     = Int -- cycles
type MemoryLine  = Vector Word32
type AccessStats = (Int, Int) --Hits/Misses
type SetIndex    = Int
type Tag         = Int

data CacheEntry  = CacheEntry {
    ceValid   :: Bool
  , ceDirty   :: Bool
  , _ceTag     :: Tag
  , ceLineIx  :: LineIx -- Até daria para ser calculado, mas economiza tempo deixar guardado
  , ceMemLine :: MemoryLine
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
newtype Set = Set (Vector CacheEntry)

instance Semigroup Set where
  (Set s1) <> (Set s2) = Set $ s1 <> s2

instance Monoid Set where
  mempty = Set mempty

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
      , stats              :: AccessStats
      , tracingEnabled     :: Bool
      , ticks              :: Int
    }
  | RAM {
        description    :: String
      , rMem           :: IntMap MemoryLine
      , latency        :: Latency
      , stats          :: AccessStats
      , rZeros         :: Vector Word32
      , tracingEnabled :: Bool
      , ticks          :: Int
      }

type Log = [String]
type MemoryAccessResponse a = (a, Log)
type MemoryLevelST = StateT MemoryLevel (Writer Log)

runMemoryLevelST :: MemoryLevelST a -> MemoryLevel -> MemoryAccessResponse (a, MemoryLevel)
runMemoryLevelST f s =  runWriter $ runStateT f s

traceMLST :: [String] -> MemoryLevelST ()
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
      Set st <- gets cSets <&> (V.! setIndex)
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
              (0, 0)
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
    (0,0)
    (V.replicate (lineSz `div` 4) 0)
    traceEnabled
    0

scoreStat :: String -> (AccessStats -> AccessStats) -> MemoryLevelST ()
scoreStat lg f = do
  logMemActivity lg
  modify $ \s -> s{stats = f (stats s)}

scoreHit :: MemoryLevelST ()
scoreHit = scoreStat "Hit" (first (+1))

scoreMiss :: MemoryLevelST ()
scoreMiss = scoreStat "Miss"(second (+1))

cacheTick :: MemoryLevelST ()
cacheTick = modify $ \s -> s{ticks= ticks s + 1}

updateCacheEntryAndTick :: SetIndex -> Int -> (CacheEntry -> CacheEntry) -> MemoryLevelST ()
updateCacheEntryAndTick setIndex wayNb updFun = do
  tick <- gets ticks
  sets <- gets cSets
  let Set st  = sets V.! setIndex
      ce      = updFun $ st V.! wayNb
      newSet  = Set $ st V.// [(wayNb, ce{ceLastAccess=tick})]
      newSets = sets V.// [(setIndex, newSet)]
  modify $ \s -> s{cSets=newSets}

touchCacheEntry :: SetIndex -> Int -> MemoryLevelST ()
touchCacheEntry setIndex wayNb = updateCacheEntryAndTick setIndex wayNb id


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
          lat <- cacheNextLevelRun $ writeLineLevel (ceLineIx ce) (ceMemLine ce)
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
  modify $ \s -> s{stats=(0,0)}

-- -> (way#, mem line)
cacheSearch :: (SetIndex, Tag) -> MemoryLevel -> Maybe (Int, MemoryLine)
cacheSearch (setIndex, tag) ~ca@Cache{} =
      (\i -> (i, ceMemLine $ st V.! i)) <$> wayNumber
      where
        (Set st) = cSets ca V.! setIndex
        wayNumber = V.findIndex (ceMatches tag) st

cacheUpdateLine :: LineIx -> (SetIndex, Tag) -> Int -> MemoryLine -> MemoryLevelST Latency
cacheUpdateLine lineIx (setIndex, tag) wayNb memLine = do
  logMemActivity $ "updating line# 0x" <> showHex32 (fromIntegral lineIx)
  updateCacheEntryAndTick setIndex wayNb (const $ CacheEntry True True tag lineIx memLine undefined)
  gets latency

cacheReplaceLine :: LineIx -> (SetIndex, Tag) -> MemoryLine -> MemoryLevelST Latency
cacheReplaceLine lineIx (setIndex, newTag) memLine = do
  logMemActivity $ "Replace to include line# 0x" <> showHex32 (fromIntegral lineIx)
  wayNb  <- gets cReplacementPolicy >>$ setIndex

  Set oldSet <- gets cSets <&> (V.! setIndex)
  let oldCe = oldSet V.! wayNb

  lat0 <- if ceNeedsWriteBack oldCe then do
      logMemActivity $ "Writing back line: 0x" <> showHex32 (fromIntegral $ ceLineIx oldCe)
      cacheNextLevelRun $ writeLineLevel (ceLineIx oldCe) (ceMemLine oldCe)
    else do
      logMemActivity "Line clean/invalid. No need to write back."
      return 0
  updateCacheEntryAndTick setIndex wayNb (const $ CacheEntry True False newTag lineIx memLine undefined)
  gets $ (+ lat0) . latency

readLineLevel :: LineIx -> MemoryLevelST (MemoryLine, Latency)
readLineLevel lnIx = do
  cacheTick
  ml <- get
  logMemActivity $ "read line# 0x" <> showHex32 (fromIntegral lnIx)
  case ml of
    RAM{} -> do
      scoreHit
      let mLine = IM.findWithDefault (rZeros ml) lnIx (rMem ml)
      gets $ \s -> (mLine, latency s)
    Cache{} -> do
      indexAndTag <- gets (`cIndexAndTag` lnIx)
      maybeMLine <- gets $ cacheSearch indexAndTag
      case maybeMLine of
        Just (mway, mLine) -> do  -- Cache Hit
          scoreHit
          touchCacheEntry (fst indexAndTag) mway
          gets $ \s -> (mLine, latency s)
        Nothing -> do -- Cache Miss
          scoreMiss
          (mLine, lat0) <- cacheNextLevelRun $ readLineLevel lnIx
          lat1 <- cacheReplaceLine lnIx indexAndTag mLine
          return (mLine, lat0 + lat1)

writeLineLevel :: LineIx -> MemoryLine -> MemoryLevelST Latency
writeLineLevel lnIx newLn = do
  cacheTick
  logMemActivity $ "write: 0x" <> showHex32 (fromIntegral lnIx)
  ml <- get
  case ml of
    RAM{} -> do
      scoreHit
      newMem <- gets $ IM.insert lnIx newLn . rMem
      state $ \s -> (latency s, s{rMem = newMem})
    Cache{} -> do
      indexAndTag <- gets (`cIndexAndTag` lnIx)
      -- all lines are first read then written by MemHierarchy
      -- so the search should always succeed and we also do not need
      -- to score a hit
      wayn  <-  gets $ fst . fromJust . cacheSearch indexAndTag
      cacheUpdateLine lnIx indexAndTag wayn newLn

nextLevel :: MemoryLevel ->  Maybe MemoryLevel
nextLevel Cache{cNextLevel=l} = Just l
nextLevel RAM{} = Nothing

-- Direct mapping
-- Fully associative
-- N-Way Set Associative

-- Replacement Policy
-- LRU
-- Random
