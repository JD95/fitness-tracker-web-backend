-- Revert backend:enum_set_intensity_create from pg

BEGIN;

DROP TYPE fitness_tracker.set_intensity;

COMMIT;
