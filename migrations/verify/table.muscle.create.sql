-- Verify backend:table.muscle.create on pg

BEGIN;

DO $$
DECLARE s_table_name VARCHAR := 'fitness_tracker.muscle';
DECLARE s_expected_columns TEXT[] := ARRAY['id', 'name'];
BEGIN
  CALL util.verify_table(s_table_name, s_expected_columns);
  ROLLBACK;
END
$$;

ROLLBACK;
