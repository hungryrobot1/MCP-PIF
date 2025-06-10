"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ServiceContextImpl = void 0;
const dal_1 = require("../dal");
const project_1 = require("./project");
const thought_1 = require("./thought");
const file_1 = require("./file");
const memory_1 = require("./memory");
const indexing_1 = require("../dal/services/indexing");
const indexing_2 = require("../dal/services/indexing");
class ServiceContextImpl {
    dal;
    projects;
    thoughts;
    files;
    memory;
    indexing;
    constructor(dal, projects, thoughts, files, memory, indexing) {
        this.dal = dal;
        this.projects = projects;
        this.thoughts = thoughts;
        this.files = files;
        this.memory = memory;
        this.indexing = indexing;
    }
    static async create() {
        const dal = (0, dal_1.getDAL)();
        if (!dal.isConnected()) {
            await dal.connect();
        }
        const projects = (0, project_1.getProjectService)();
        const thoughts = (0, thought_1.getThoughtService)();
        const files = (0, file_1.getFileService)();
        const memory = (0, memory_1.getMemoryService)();
        // Create IndexingService with DAL
        const fileFilter = new indexing_2.FileFilter();
        const db = dal.database.getDatabase();
        if (!db) {
            throw new Error('Database not connected');
        }
        const indexing = new indexing_1.IndexingService(db, dal, fileFilter);
        return new ServiceContextImpl(dal, projects, thoughts, files, memory, indexing);
    }
    async cleanup() {
        // Stop all file watchers
        await this.indexing.stopAllWatchers();
        if (this.dal.isConnected()) {
            await this.dal.disconnect();
        }
    }
}
exports.ServiceContextImpl = ServiceContextImpl;
//# sourceMappingURL=context.js.map