-- Revert backend:procedure_verify_table_create from pg

BEGIN;

DROP PROCEDURE IF EXISTS util.verify_table;

COMMIT;
