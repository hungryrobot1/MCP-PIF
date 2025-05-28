# MCP-PIF Development Roadmap

## Vision
Transform MCP-PIF from a CLI-based personal intelligence framework into a fully-featured MCP server that provides AI assistants with sophisticated file system access, semantic search, and knowledge management capabilities.

## Current Status

### ✅ Completed
- Core domain logic (Project, Permission, Document services)
- Data Access Layer with Result-based error handling
- ML module with FastAPI for embeddings
- Basic CLI interface
- SQLite + ChromaDB integration
- Transaction-based consistency

### 🚧 In Progress
- Command feedback for ML operations
- Search command refinement
- ML module testing

### ❌ Not Started
- MCP protocol implementation
- Knowledge graph features
- Personas system
- Advanced visualizations

## Development Phases

### Phase 1: Core Validation & CLI Completion *(Current Phase)*
**Goal**: Ensure all workflows are solid through CLI before MCP integration

#### 1.1 Command Structure Refactoring (Immediate)
- [ ] Fix command alias collisions
  - `status` → `status`, `st` (remove 's')
  - `search` → `search`, `s`, `find`, `f`
  - `new` → `add`, `a`
- [ ] Implement `info <project>` command for detailed project status
- [ ] Remove/consolidate redundant commands

#### 1.2 CLI State Machine Implementation
- [ ] Implement mode-based command structure (PROJECT, SEARCH modes)
- [ ] Add mode switching commands
- [ ] Create mode-specific command registries
- [ ] Add context preservation between modes

#### 1.3 ML Pipeline Validation
- [ ] Add progress indicators for embedding generation
- [ ] Implement indexing status tracking
- [ ] Add feedback for ML service operations
- [ ] Validate embedding updates on file changes
- [ ] Add comprehensive ML module tests

#### 1.4 Search Enhancement
- [ ] Fix search command implementation
- [ ] Add multiple search types (semantic, SQL, graph)
- [ ] Implement unified result ranking
- [ ] Add search result filtering

### Phase 2: MCP Protocol Implementation
**Goal**: Expose validated functionality through MCP

#### 2.1 MCP Server Architecture
- [ ] Design tool schemas for MCP
- [ ] Implement MCP protocol handlers
- [ ] Create MCP-to-domain service bridge
- [ ] Handle MCP lifecycle events

#### 2.2 Tool Implementation
- [ ] File operations (read, write, explore)
- [ ] Project management tools
- [ ] Search tools (unified interface)
- [ ] Status and monitoring tools

#### 2.3 Data Transformation
- [ ] Result<T> to MCP response mapping
- [ ] Error handling and reporting
- [ ] Streaming for large responses
- [ ] Progress reporting for long operations

#### 2.4 Testing & Optimization
- [ ] Integration with Claude Desktop
- [ ] Performance optimization
- [ ] Error handling refinement
- [ ] Documentation for Claude usage

### Phase 3: Advanced Features
**Goal**: Differentiated capabilities beyond basic file access

#### 3.1 Knowledge Management
- [ ] Journaling system implementation
- [ ] Reasoning trace capture
- [ ] Cross-reference extraction
- [ ] Knowledge graph construction

#### 3.2 Unified Memory System
- [ ] Multi-modal search fusion
- [ ] Temporal proximity search
- [ ] Version-aware search
- [ ] Graph-enhanced results

#### 3.3 Personas & Prompting
- [ ] Persona definition system
- [ ] Context injection mechanism
- [ ] Role-based tool access
- [ ] Dynamic prompt enhancement

#### 3.4 Advanced Visualizations
- [ ] Project structure visualization
- [ ] Knowledge graph explorer
- [ ] Search result clustering
- [ ] Embedding space visualization

## Implementation Strategy

### Principles
1. **CLI First**: Validate all workflows in CLI before MCP
2. **Test Driven**: Comprehensive tests before features
3. **Incremental**: Small, working increments
4. **Claude-Optimized**: Design for AI assistant usage patterns

### Collaboration Workflow
1. Documentation-driven development
2. Strategic planning in this environment
3. Detailed implementation with Claude Code
4. Review and iteration cycles
5. Progress tracking via documentation updates

## Success Metrics

### Phase 1
- [ ] All CLI commands working without errors
- [ ] ML pipeline provides clear feedback
- [ ] Search returns relevant results quickly
- [ ] 90%+ test coverage on critical paths

### Phase 2
- [ ] MCP server runs in Claude Desktop
- [ ] All core operations available as tools
- [ ] Response times < 500ms for common operations
- [ ] Clear error messages for Claude

### Phase 3
- [ ] Knowledge graph with 1000+ relationships
- [ ] Unified search improves result relevance by 50%
- [ ] Personas enhance context understanding
- [ ] Visualization tools aid debugging

## Next Immediate Actions

1. Create detailed implementation specs in `/docs`
2. Fix command alias collisions
3. Implement project info command
4. Add ML operation feedback
5. Write comprehensive tests
