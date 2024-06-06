{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Queries.Squeal where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Db.Schema as Db
import qualified Db.Workout as Db (Workout)
import Squeal.PostgreSQL hiding (execute)
import qualified Squeal.PostgreSQL as Squeal

execute :: (MonadIO m, MonadPQ Db.Schema m) => Statement Db.Schema () a -> m [a]
execute stmt = getRows =<< Squeal.execute stmt

allWorkouts :: Statement Db.Schema () Db.Workout
allWorkouts =
  query $
    select_
      ( #workout ! #id `as` #id
          :* #workout ! #name `as` #name
      )
      (from $ table $ #fitness_tracker ! #workout)
