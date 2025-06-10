"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.DatabaseMigrator = void 0;
class DatabaseMigrator {
    db;
    constructor(db) {
        this.db = db;
        // Create the migrations tracking table on instantiation
        // This table records which migrations have been applied
        this.ensureMigrationsTable();
    }
    ensureMigrationsTable() {
        // This table tracks migration history
        // It's prefixed with _ to indicate it's a system table
        this.db.exec(`
      CREATE TABLE IF NOT EXISTS _migrations (
        version INTEGER PRIMARY KEY,
        name TEXT NOT NULL,
        applied_at TEXT NOT NULL DEFAULT (datetime('now'))
      )
    `);
    }
    getCurrentVersion() {
        // Find the highest migration version that has been applied
        const row = this.db.prepare('SELECT MAX(version) as version FROM _migrations').get();
        return row.version || 0; // Return 0 if no migrations have run
    }
    async runMigrations(migrations) {
        const currentVersion = this.getCurrentVersion();
        // Filter to only migrations newer than current version
        // Sort by version to ensure correct order
        const pendingMigrations = migrations
            .filter(m => m.version > currentVersion)
            .sort((a, b) => a.version - b.version);
        if (pendingMigrations.length === 0) {
            console.log('✓ Database is up to date');
            return;
        }
        console.log(`📦 Running ${pendingMigrations.length} migrations...`);
        for (const migration of pendingMigrations) {
            try {
                console.log(`  Running migration ${migration.version}: ${migration.name}`);
                // Run each migration in a transaction for safety
                // If migration fails, the database remains unchanged
                this.db.transaction(() => {
                    migration.up(this.db);
                    // Record successful migration
                    this.db.prepare('INSERT INTO _migrations (version, name) VALUES (?, ?)').run(migration.version, migration.name);
                })();
                console.log(`  ✓ Migration ${migration.version} complete`);
            }
            catch (error) {
                // Stop on first error to prevent cascading failures
                console.error(`  ❌ Migration ${migration.version} failed:`, error);
                throw error;
            }
        }
        console.log('✓ All migrations complete');
    }
}
exports.DatabaseMigrator = DatabaseMigrator;
//# sourceMappingURL=migrator.js.map