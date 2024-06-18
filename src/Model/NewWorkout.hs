{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Model.NewWorkout where

import Data.Text
import Data.Time.Clock
import Data.UUID
import GHC.Generics
import Model.SetIntensity

data NewWorkout = NewWorkout
  { name :: Text
  }
  deriving stock (Show, Eq, Generic)
