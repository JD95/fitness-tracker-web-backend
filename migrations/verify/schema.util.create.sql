-- Verify backend:schema_util_create on pg

BEGIN;

DO $$
BEGIN
   ASSERT (SELECT has_schema_privilege('util', 'usage'));
END $$;

ROLLBACK;
