-- Deploy backend:table.primary_muscle.create to pg

BEGIN;

CREATE TABLE fitness_tracker.primary_muscle
  ( id UUID PRIMARY KEY DEFAULT gen_random_uuid()
  , workout UUID NOT NULL
  , muscle UUID NOT NULL
  , CONSTRAINT fk_workout_id FOREIGN KEY(workout) REFERENCES fitness_tracker.workout(id)
  , CONSTRAINT fk_muscle_id FOREIGN KEY(muscle) REFERENCES fitness_tracker.muscle(id)
  );

COMMIT;
