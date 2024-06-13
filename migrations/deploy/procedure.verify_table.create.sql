-- Deploy backend:procedure_verify_table_create to pg

BEGIN;

-- XXX Add DDLs here.

CREATE OR REPLACE PROCEDURE util.verify_table
  ( PS_TABLE_NAME VARCHAR
  , PS_EXPECTED_COLUMNS VARCHAR []
  )
LANGUAGE 'plpgsql'
AS $$
DECLARE
  b_result BOOLEAN := FALSE;
  s_missing_columns VARCHAR[];
  s_extra_columns VARCHAR[];
BEGIN

  -- Verify the table exists
  BEGIN
    EXECUTE FORMAT('SELECT TRUE FROM %s WHERE FALSE', PS_TABLE_NAME);
  EXCEPTION
    WHEN OTHERS
    THEN RAISE EXCEPTION 'The table "%" does not exist.', PS_TABLE_NAME;
  END;

  WITH c AS (
    SELECT c.column_name
    FROM information_schema.columns c
    WHERE CONCAT(c.table_schema, '.', c.table_name) = PS_TABLE_NAME
  )
  SELECT
    MIN(CASE WHEN e IS NULL OR c.column_name IS NULL THEN 0 ELSE 1 END)::BOOLEAN AS passed_test
  , ARRAY_REMOVE(ARRAY_AGG(CASE WHEN c.column_name IS NULL THEN e END), NULL) AS missing_columns
  , ARRAY_REMOVE(ARRAY_AGG(CASE WHEN e IS NULL THEN c.column_name END), NULL) AS extra_columns
  INTO
    b_result
  , s_missing_columns
  , s_extra_columns
  FROM UNNEST(PS_EXPECTED_COLUMNS) e
    FULL JOIN c ON e = c.column_name;

  IF b_result = FALSE
  THEN RAISE EXCEPTION 'The table "%" does not match the expected definition. Expected: "%"; Missing: "%"; Extra: "%";', PS_TABLE_NAME, PS_EXPECTED_COLUMNS, s_missing_columns, s_extra_columns;
  END IF;

END
$$;

COMMIT;
