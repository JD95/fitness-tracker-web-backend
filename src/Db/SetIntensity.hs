{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Db.SetIntensity where

import Data.Text (Text)
import Data.UUID
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import qualified Model.SetIntensity as Model
import Squeal.PostgreSQL

type Labels = '["no_effort", "easy", "medium", "hard", "failure"]

type PGsetintensity = 'PGenum Labels

type Schema = "set_intensity" ::: 'Typedef PGsetintensity

newtype SetIntensity
  = SetIntensity Model.SetIntensity
  deriving newtype (Eq, Show, Enum, Bounded)

instance IsPG SetIntensity where
  type PG SetIntensity = PGsetintensity

instance FromPG SetIntensity where
  fromPG =
    enumValue $
      label @"no_effort" (SetIntensity Model.NoEffort)
        :* label @"easy" (SetIntensity Model.Easy)
        :* label @"medium" (SetIntensity Model.Medium)
        :* label @"hard" (SetIntensity Model.Hard)
        :* label @"failure" (SetIntensity Model.Failure)

instance ToPG db SetIntensity where
  toPG = enumParam $ \case
    (SetIntensity Model.NoEffort) -> label @"no_effort"
    (SetIntensity Model.Easy) -> label @"easy"
    (SetIntensity Model.Medium) -> label @"medium"
    (SetIntensity Model.Hard) -> label @"hard"
    (SetIntensity Model.Failure) -> label @"failure"
