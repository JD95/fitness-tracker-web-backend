-- Verify backend:enum_set_intensity_create on pg

BEGIN;

DO $$
DECLARE s_enum_name VARCHAR := 'fitness_tracker.set_intensity';
DECLARE s_expected_values TEXT[] := '{no_effort, easy, medium, hard, failure}';
BEGIN
  CALL util.verify_enum(s_enum_name, s_expected_values);
  ROLLBACK;
END
$$;


ROLLBACK;
