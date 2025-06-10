// Export all migrations in order
// The order here doesn't matter - they'll be sorted by version number
export { addIndexingColumnsMigration } from './001-add-indexing-columns';
export { createFileIndexTableMigration } from './002-create-file-index-table';
export { handleActiveConstraintMigration } from './003-handle-active-constraint';