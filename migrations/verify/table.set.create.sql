-- Verify backend:table_set_create on pg

BEGIN;

DO $$
DECLARE s_table_name VARCHAR := 'fitness_tracker.set';
DECLARE s_expected_columns TEXT[] := ARRAY['id', 'workout', 'reps', 'time', 'weight', 'intensity'];
BEGIN
  CALL util.verify_table(s_table_name, s_expected_columns);
  ROLLBACK;
END
$$;

ROLLBACK;
