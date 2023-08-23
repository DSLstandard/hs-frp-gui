module Vg.Frp where

import qualified Data.Map.Strict as Map
import           Data.MonadicStreamFunction.InternalCore
import qualified Data.Stream as S
import           FRP.BearRiver
import           Relude
import           Vg.Utils
import Control.Lens

dictCaching :: (Monad m, Ord a) => (a -> m b) -> MSF m a b
dictCaching load =
  feedback Map.empty $ arrM \(k, cache) -> do
    case Map.lookup k cache of
      Nothing -> do
        v <- load k
        pure (v, Map.insert k v cache)
      Just v -> do
        pure (v, cache)

diffEdge :: (Eq a, Monad m) => a -> MSF m a (Event a)
diffEdge i = feedback i $ arr \(now, last) ->
  Relude.first (guard (now /= last) $>) $ dup now

-- pSwitch :: (Monad m, Traversable f)
--          => (forall sf. (a -> f sf -> f (b, sf)))
--          -> f (MSF m b c)
--          -> MSF m (a, f c) (Event d)
--          -> (f (MSF m b c) -> d -> MSF m a (f c))
--          -> MSF m a (f c)
-- pSwitch route b2cs deciding evolve =
--   MSF \a -> do
--     (cs, b2cs) <- unzipF <$> forM (route a b2cs) \(b, b2c) -> unMSF b2c b
--     (may_d, deciding) <- unMSF deciding (a, cs)
--     case may_d of
--       NoEvent -> pure (cs, pSwitch route b2cs deciding evolve)
--       Event d -> evolve b2cs d

dpSwitchExposed :: (Monad m, Traversable f)
         => (a -> f (MSF m b c) -> f (b, MSF m b c))
         -> f (MSF m b c)
         -> MSF m (a, f c) (Event d)
         -> (f (MSF m b c) -> d -> MSF m a (f c))
         -> MSF m a (f c)
dpSwitchExposed route b2cs deciding evolve = MSF \a -> do
  (cs, b2cs) <- unzipF <$> forM (route a b2cs) \(b, b2c) -> unMSF b2c b
  (may_d, deciding) <- unMSF deciding (a, cs)
  pure (cs, event (dpSwitchExposed route b2cs deciding evolve) (evolve b2cs) may_d)

dpSwitch :: (Monad m, Traversable f)
         => (forall sf. (a -> f sf -> f (b, sf)))
         -> f (MSF m b c)
         -> MSF m (a, f c) (Event d)
         -> (f (MSF m b c) -> d -> MSF m a (f c))
         -> MSF m a (f c)
dpSwitch route = dpSwitchExposed route

parExposed :: (Monad m, Traversable f)
  => (a -> f (MSF m b c) -> f (b, MSF m b c))
  -> f (MSF m b c)
  -> MSF m a (f c)
parExposed route b2cs =
  dpSwitchExposed route b2cs (arr (const (NoEvent @Void))) (const absurd)

dynamicParC :: Monad m => MSF m a b -> MSF m [a] [b]
dynamicParC initialsf = parExposed (\as sfs -> zip as (sfs <> repeat initialsf)) []

------------------------------------------------------------------------
-- * Misc
------------------------------------------------------------------------

iterThru :: Monad m => Time -> S.Stream a -> SF m xxx a
iterThru avgt stream = feedback (imap (\i x -> (avgt*fromIntegral i, x)) stream)
  proc (_, xs) -> do
    t <- () >- localTime
    let xs' = S.dropWhile ((< t) . fst) xs
    returnA -< (snd $ S.head xs', xs')