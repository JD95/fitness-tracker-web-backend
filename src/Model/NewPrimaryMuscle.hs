{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Model.NewPrimaryMuscle where

import Data.UUID
import GHC.Generics
import Model.SetIntensity

data NewPrimaryMuscle = NewPrimaryMuscle
  { workout :: UUID,
    muscle :: UUID
  }
  deriving stock (Show, Eq, Generic)
