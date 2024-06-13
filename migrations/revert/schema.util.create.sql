-- Revert backend:schema_util_create from pg

BEGIN;

DROP SCHEMA IF EXISTS util;

COMMIT;
