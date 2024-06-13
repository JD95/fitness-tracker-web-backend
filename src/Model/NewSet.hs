{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Model.NewSet where

import Data.Text
import Data.Time.Clock
import Data.UUID
import GHC.Generics
import Model.SetIntensity

data NewSet = NewSet
  { workout :: UUID,
    reps :: Int,
    time :: UTCTime,
    weight :: Float,
    intensity :: SetIntensity
  }
  deriving stock (Show, Eq, Generic)
