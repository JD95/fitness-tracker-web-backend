-- Revert backend:workouts from pg

BEGIN;

DROP TABLE fitness_tracker.workout;

COMMIT;
