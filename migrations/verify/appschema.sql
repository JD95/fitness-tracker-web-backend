-- Verify backend:appschema on pg

BEGIN;

DO $$
BEGIN
   ASSERT (SELECT has_schema_privilege('fitness_tracker', 'usage'));
END $$;

ROLLBACK;
