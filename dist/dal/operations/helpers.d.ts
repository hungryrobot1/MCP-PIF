export declare function generateId(): string;
export declare function generatePreview(content: string): string;
export declare function countWords(content: string): number;
export declare function validateAlias(alias: string): boolean;
export declare function validateProjectPath(path: string): {
    valid: boolean;
    reason?: string;
};
export declare function validateDocumentPath(path: string): {
    valid: boolean;
    reason?: string;
};
export declare function validateContentHash(hash: string): boolean;
//# sourceMappingURL=helpers.d.ts.map