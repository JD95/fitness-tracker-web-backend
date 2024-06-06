-- Verify backend:workouts on pg

BEGIN;

SELECT id, name
FROM fitness_tracker.workout
WHERE FALSE;

ROLLBACK;
