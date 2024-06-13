{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Model.SetIntensity where

import GHC.Generics

data SetIntensity
  = NoEffort
  | Easy
  | Medium
  | Hard
  | Failure
  deriving stock (Eq, Show, Generic, Enum, Bounded)
