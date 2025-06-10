"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.handleActiveConstraintMigration = exports.createFileIndexTableMigration = exports.addIndexingColumnsMigration = void 0;
// Export all migrations in order
// The order here doesn't matter - they'll be sorted by version number
var _001_add_indexing_columns_1 = require("./001-add-indexing-columns");
Object.defineProperty(exports, "addIndexingColumnsMigration", { enumerable: true, get: function () { return _001_add_indexing_columns_1.addIndexingColumnsMigration; } });
var _002_create_file_index_table_1 = require("./002-create-file-index-table");
Object.defineProperty(exports, "createFileIndexTableMigration", { enumerable: true, get: function () { return _002_create_file_index_table_1.createFileIndexTableMigration; } });
var _003_handle_active_constraint_1 = require("./003-handle-active-constraint");
Object.defineProperty(exports, "handleActiveConstraintMigration", { enumerable: true, get: function () { return _003_handle_active_constraint_1.handleActiveConstraintMigration; } });
//# sourceMappingURL=index.js.map