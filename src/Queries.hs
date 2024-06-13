module Queries where

import Db.Workout

class Queries m where
  allWorkouts :: m [Workout]
