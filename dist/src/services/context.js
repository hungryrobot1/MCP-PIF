"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ServiceContextImpl = void 0;
const dal_1 = require("../dal");
const ml_client_1 = require("./ml-client");
const project_1 = require("./project");
const thought_1 = require("./thought");
const file_1 = require("./file");
const memory_1 = require("./memory");
class ServiceContextImpl {
    dal;
    ml;
    projects;
    thoughts;
    files;
    memory;
    constructor(dal, ml, projects, thoughts, files, memory) {
        this.dal = dal;
        this.ml = ml;
        this.projects = projects;
        this.thoughts = thoughts;
        this.files = files;
        this.memory = memory;
    }
    static async create() {
        const dal = (0, dal_1.getDAL)();
        if (!dal.isConnected()) {
            await dal.connect();
        }
        const ml = (0, ml_client_1.getMLClient)();
        const projects = (0, project_1.getProjectService)();
        const thoughts = (0, thought_1.getThoughtService)();
        const files = (0, file_1.getFileService)();
        const memory = (0, memory_1.getMemoryService)();
        return new ServiceContextImpl(dal, ml, projects, thoughts, files, memory);
    }
    async cleanup() {
        if (this.dal.isConnected()) {
            await this.dal.disconnect();
        }
    }
}
exports.ServiceContextImpl = ServiceContextImpl;
//# sourceMappingURL=context.js.map