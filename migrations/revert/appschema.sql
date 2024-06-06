-- Revert backend:appschema from pg

BEGIN;

DROP SCHEMA fitness_tracker;

COMMIT;
