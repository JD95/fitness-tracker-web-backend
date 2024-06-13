-- Deploy backend:workouts to pg
-- requires: appschema

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE fitness_tracker.workout
  ( id UUID PRIMARY KEY DEFAULT gen_random_uuid()
  , name TEXT NOT NULL
  );

COMMIT;
