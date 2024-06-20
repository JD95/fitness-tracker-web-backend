-- Revert backend:table.muscle.create from pg

BEGIN;

DROP TABLE IF EXISTS fitness_tracker.muscle;

COMMIT;
