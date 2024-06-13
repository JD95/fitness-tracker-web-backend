-- Revert backend:procedure.verify_enum.create from pg

BEGIN;

DROP PROCEDURE IF EXISTS util.verify_enum;

COMMIT;
