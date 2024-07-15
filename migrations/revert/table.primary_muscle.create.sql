-- Revert backend:table.primary_muscle.create from pg

BEGIN;

DROP TABLE fitness_tracker.primary_muscle;

COMMIT;
