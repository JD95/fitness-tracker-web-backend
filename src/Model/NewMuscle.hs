{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Model.NewMuscle where

import Data.Text
import Data.Time.Clock
import Data.UUID
import GHC.Generics
import Model.SetIntensity

data NewMuscle = NewMuscle
  { name :: Text
  }
  deriving stock (Show, Eq, Generic)
