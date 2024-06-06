{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Db.Workout where

import Data.Text (Text)
import Data.UUID
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL

type Columns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGuuid,
     "name" ::: 'NoDef :=> 'NotNull 'PGtext
   ]

type Constraints =
  '["pk_workout" ::: 'PrimaryKey '["id"]]

type Schema = "workout" ::: 'Table (Constraints :=> Columns)

data Workout = Workout {id :: UUID, name :: Text}
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
