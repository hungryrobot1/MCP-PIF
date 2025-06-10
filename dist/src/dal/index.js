"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.DatabaseConnection = exports.DataAccessLayer = void 0;
exports.getDAL = getDAL;
const connection_1 = require("./database/connection");
const projects_1 = require("./operations/projects");
const documents_1 = require("./operations/documents");
const thoughts_1 = require("./operations/thoughts");
const filesystem_1 = require("./operations/filesystem");
const result_1 = require("../types/result");
const errors_1 = require("../types/errors");
const ml_client_1 = require("./ml-client");
class DataAccessLayer {
    db;
    projects;
    documents;
    thoughts;
    filesystem;
    mlClient;
    get database() {
        return this.db;
    }
    constructor(dbPath) {
        this.db = (0, connection_1.getDatabaseConnection)(dbPath);
        this.projects = new projects_1.ProjectOperations(this.db);
        this.documents = new documents_1.DocumentOperations(this.db);
        this.thoughts = new thoughts_1.ThoughtOperations(this.db);
        this.filesystem = new filesystem_1.FileOperations();
        this.mlClient = (0, ml_client_1.getMLClient)();
    }
    async connect() {
        return this.db.connect();
    }
    async disconnect() {
        return this.db.disconnect();
    }
    isConnected() {
        return this.db.isConnected();
    }
    async transaction(fn) {
        if (!this.isConnected()) {
            return result_1.Result.err(new errors_1.DatabaseError('transaction', new Error('Database not connected')));
        }
        try {
            const result = await fn();
            return result;
        }
        catch (error) {
            return result_1.Result.err(error instanceof Error ? error : new Error(String(error)));
        }
    }
}
exports.DataAccessLayer = DataAccessLayer;
// Export types and helpers
__exportStar(require("./operations/types"), exports);
__exportStar(require("./operations/helpers"), exports);
var connection_2 = require("./database/connection");
Object.defineProperty(exports, "DatabaseConnection", { enumerable: true, get: function () { return connection_2.DatabaseConnection; } });
// Singleton instance
let instance = null;
function getDAL(dbPath) {
    if (!instance) {
        instance = new DataAccessLayer(dbPath);
    }
    return instance;
}
//# sourceMappingURL=index.js.map