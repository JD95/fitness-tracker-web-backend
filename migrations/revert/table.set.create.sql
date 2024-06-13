-- Revert backend:table_set_create from pg

BEGIN;

DROP TYPE fitness_tracker.set;

COMMIT;
