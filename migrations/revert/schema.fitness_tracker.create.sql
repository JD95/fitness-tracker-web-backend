-- Revert backend:schema_fitness_tracker_create from pg

BEGIN;

DROP SCHEMA IF EXISTS fitness_tracker;

COMMIT;
