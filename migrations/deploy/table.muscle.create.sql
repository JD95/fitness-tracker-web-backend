-- Deploy backend:table.muscle.create to pg

BEGIN;

CREATE TABLE fitness_tracker.muscle
  ( id UUID PRIMARY KEY DEFAULT gen_random_uuid()
  , name TEXT NOT NULL
  );

COMMIT;
