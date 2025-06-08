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
exports.MemoryService = void 0;
exports.getMemoryService = getMemoryService;
const service_1 = require("./service");
Object.defineProperty(exports, "MemoryService", { enumerable: true, get: function () { return service_1.MemoryService; } });
let instance = null;
function getMemoryService(config) {
    if (!instance) {
        instance = new service_1.MemoryService(config);
    }
    return instance;
}
__exportStar(require("./types"), exports);
//# sourceMappingURL=index.js.map