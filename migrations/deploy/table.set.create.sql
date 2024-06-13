-- Deploy backend:table_set_create to pg
-- requires: appschema

BEGIN;

CREATE TABLE fitness_tracker.set
  ( id UUID PRIMARY KEY DEFAULT gen_random_uuid()
  , workout UUID NOT NULL
  , reps INT NOT NULL
  , time TIMESTAMPTZ NOT NULL
  , weight FLOAT4 NOT NULL
  , intensity fitness_tracker.set_intensity NOT NULL
  );

COMMIT;
