export interface TableSchema {
    name: string;
    sql: string;
    indexes: string[];
    dependencies: string[];
}
export declare const projectsTableSchema: TableSchema;
export declare const documentsTableSchema: TableSchema;
export declare const thoughtsTableSchema: TableSchema;
export declare const fileIndexTableSchema: TableSchema;
export declare const documentsFTSSchema = "\n  CREATE VIRTUAL TABLE IF NOT EXISTS documents_fts USING fts5(\n    path,\n    content,\n    content=documents,\n    content_rowid=rowid\n  )\n";
export declare const thoughtsFTSSchema = "\n  CREATE VIRTUAL TABLE IF NOT EXISTS thoughts_fts USING fts5(\n    content,\n    content=thoughts,\n    content_rowid=rowid\n  )\n";
export declare const tableSchemas: TableSchema[];
//# sourceMappingURL=schema.d.ts.map