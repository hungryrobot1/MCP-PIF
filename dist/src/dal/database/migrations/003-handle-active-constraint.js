"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.handleActiveConstraintMigration = void 0;
exports.handleActiveConstraintMigration = {
    version: 3,
    name: 'Handle is_active unique constraint',
    up: (db) => {
        // This migration ensures only one project can be active at a time
        // We do this in a separate migration to handle existing databases gracefully
        try {
            // First, ensure only one project is marked as active
            // Get all active projects
            const activeProjects = db.prepare('SELECT id FROM projects WHERE is_active = 1').all();
            if (activeProjects.length > 1) {
                // Keep only the first one active, deactivate others
                const keepActive = activeProjects[0];
                db.prepare('UPDATE projects SET is_active = 0 WHERE is_active = 1 AND id != ?')
                    .run(keepActive.id);
            }
            // Now we can safely create the unique index
            // Using CREATE INDEX IF NOT EXISTS to make it idempotent
            db.exec(`
        CREATE UNIQUE INDEX IF NOT EXISTS idx_projects_active 
        ON projects(is_active) 
        WHERE is_active = 1
      `);
        }
        catch (error) {
            // If the constraint already exists in some form, that's fine
            if (!error.message.includes('already exists')) {
                throw error;
            }
        }
    }
};
//# sourceMappingURL=003-handle-active-constraint.js.map