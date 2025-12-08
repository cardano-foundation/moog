module Core.Types.Duration
    ( Duration (..)
    , durationToTimeDiff
    , negateDuration
    , durationToNominalDiffTime
    )
where

import Control.Applicative (Alternative, (<|>))
import Data.Function (on)
import Data.Time (NominalDiffTime)
import Lib.JSON.Canonical.Extra
    ( getIntegralField
    , intJSON
    , object
    )
import System.Time (TimeDiff (tdHour, tdMin), noTimeDiff)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (JSNum)
    , ReportSchemaErrors
    , ToJSON (..)
    , expectedButGotValue
    )

data Duration = Hours Int | Minutes Int
    deriving (Show)

instance Monoid Duration where
    mempty = Minutes 0

instance Semigroup Duration where
    Hours h1 <> Hours h2 = Hours (h1 + h2)
    Minutes m1 <> Minutes m2 = Minutes (m1 + m2)
    Hours h <> Minutes m = Minutes (h * 60 + m)
    Minutes m <> Hours h = Minutes (m + h * 60)

durationToNominalDiffTime :: Duration -> NominalDiffTime
durationToNominalDiffTime (Hours h) = fromIntegral (h * 3600)
durationToNominalDiffTime (Minutes m) = fromIntegral (m * 60)

durationMinutes :: Duration -> Int
durationMinutes (Hours h) = h * 60
durationMinutes (Minutes m) = m
instance Eq Duration where
    (==) = (==) `on` durationMinutes

instance Ord Duration where
    compare = compare `on` durationMinutes

instance Monad m => ToJSON m Duration where
    toJSON (Hours h) = object [("hours", intJSON h)]
    toJSON (Minutes m) = object [("minutes", intJSON m)]

instance (Alternative m, ReportSchemaErrors m) => FromJSON m Duration where
    fromJSON v = directHours v <|> specific v
      where
        directHours (JSNum n) = pure $ Hours (fromIntegral n)
        directHours w = expectedButGotValue "Duration as number of hours" w
        specific obj = do
            mapping <- fromJSON obj
            hours mapping <|> minutes mapping
        hours mapping = Hours <$> getIntegralField "hours" mapping
        minutes mapping = Minutes <$> getIntegralField "minutes" mapping

durationToTimeDiff :: Duration -> TimeDiff
durationToTimeDiff (Hours h) = noTimeDiff{tdHour = h}
durationToTimeDiff (Minutes m) = noTimeDiff{tdMin = m}

negateDuration :: Duration -> Duration
negateDuration (Hours h) = Hours (negate h)
negateDuration (Minutes m) = Minutes (negate m)
