-- Deploy backend:procedure.verify_enum.create to pg

BEGIN;

CREATE OR REPLACE PROCEDURE util.verify_enum
  ( PS_ENUM_NAME VARCHAR
  , PS_EXPECTED_VALUES VARCHAR []
  )
LANGUAGE 'plpgsql'
AS $$
DECLARE
  b_result BOOLEAN := FALSE;
  s_values VARCHAR [];
BEGIN
  BEGIN
    EXECUTE FORMAT('SELECT ENUM_RANGE(NULL::%s)', PS_ENUM_NAME)
    INTO s_values;
  EXCEPTION
    WHEN OTHERS
    THEN RAISE EXCEPTION 'The enum "%" does not exist.', PS_ENUM_NAME;
  END;

  SELECT (PS_EXPECTED_VALUES = s_values)
  INTO b_result;

  IF b_result = FALSE
  THEN RAISE EXCEPTION 'The enum "%" does not match the expected definition. Expected "%"; Current: "%";', PS_ENUM_NAME, PS_EXPECTED_VALUES, s_values;
  END IF;

END
$$;

COMMIT;
