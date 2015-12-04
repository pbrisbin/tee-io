-- |
--
-- TODO: pick better names and release this as a small library
--
module Data.Time.Duration
    ( since
    , priorTo
    , module Data.Time
    , module Data.Time.Units
    ) where

import Prelude
import Data.Time hiding (Day)
import Data.Time.Units

-- | Calculate a relative time since the given one
--
-- > (5 :: Second) `since` now
--
since :: TimeUnit a => a -> UTCTime -> UTCTime
since s = addUTCTime $ toNominalDiffTime s

-- | Calculate a relative time prior to the given one
--
-- > (10 :: Day) `priorTo` now
--
priorTo :: TimeUnit a => a -> UTCTime -> UTCTime
priorTo s = addUTCTime (negate $ toNominalDiffTime s)

toNominalDiffTime :: TimeUnit a => a -> NominalDiffTime
toNominalDiffTime = fromInteger . (`div` 1000000) . toMicroseconds
