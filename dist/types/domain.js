"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.projectRecordToProject = projectRecordToProject;
exports.documentRecordToDocument = documentRecordToDocument;
exports.thoughtRecordToThought = thoughtRecordToThought;
// Converter functions
function projectRecordToProject(record) {
    return {
        id: record.id,
        alias: record.alias,
        name: record.name,
        rootPath: record.root_path,
        createdAt: new Date(record.created_at),
        settings: JSON.parse(record.settings)
    };
}
function documentRecordToDocument(record) {
    return {
        id: record.id,
        projectId: record.project_id,
        path: record.path,
        contentHash: record.content_hash,
        size: record.size,
        lastIndexed: new Date(record.last_indexed),
        modifiedAt: new Date(record.modified_at),
        hasEmbedding: record.has_embedding === 1
    };
}
function thoughtRecordToThought(record) {
    return {
        id: record.id,
        content: record.content,
        preview: record.preview,
        wordCount: record.word_count,
        createdAt: new Date(record.created_at),
        updatedAt: record.updated_at ? new Date(record.updated_at) : null,
        projectId: record.project_id
    };
}
//# sourceMappingURL=domain.js.map