-- Deploy backend:enum_set_intensity_create to pg
-- requires: appschema

BEGIN;

CREATE TYPE fitness_tracker.set_intensity AS ENUM ('no_effort', 'easy', 'medium', 'hard', 'failure');

COMMIT;
