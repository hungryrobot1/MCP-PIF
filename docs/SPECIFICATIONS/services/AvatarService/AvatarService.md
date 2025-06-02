# Avatar Service Specification (JSON-based)

## Overview

The AvatarService manages AI personas (avatars) stored as a simple JSON file. Each avatar is a name-prompt pair that modifies conversation style.

## Design Principles

1. **File-based Storage**: Avatars stored in avatars.json
2. **Direct Editing**: Users can edit the JSON file directly
3. **No Database**: Removes complexity of SQLite table
4. **Version Control Friendly**: Changes trackable in git
5. **Portable**: Easy to backup and share avatar collections

## Service Interface

```typescript
export interface IAvatarService {
  // CRUD operations
  createAvatar(name: string, prompt: string): Promise<Result<Avatar>>;
  getAvatar(name: string): Promise<Result<Avatar | null>>;
  updateAvatar(name: string, prompt: string): Promise<Result<Avatar>>;
  deleteAvatar(name: string): Promise<Result<void>>;
  
  // List operations
  listAvatarNames(): Promise<Result<string[]>>;
  
  // File operations
  loadAvatars(): Promise<Result<void>>;
  saveAvatars(): Promise<Result<void>>;
}
```

## Type Definitions

```typescript
export interface Avatar {
  name: string;
  prompt: string;
}

export interface AvatarStore {
  [name: string]: {
    prompt: string;
    isDefault?: boolean;
  };
}
```

## Implementation

```typescript
export class AvatarService implements IAvatarService {
  private avatars: AvatarStore = {};
  private avatarsPath: string;
  private hasChanges: boolean = false;

  constructor(private fileOps: IFileOperations, avatarsPath: string) {
    this.avatarsPath = avatarsPath;
  }

  async loadAvatars(): Promise<Result<void>> {
    try {
      const result = await this.fileOps.read(this.avatarsPath);
      
      if (result.ok) {
        const content = result.value.toString('utf-8');
        this.avatars = JSON.parse(content);
      } else if (result.error.code === 'ENOENT') {
        // File doesn't exist, start with empty avatars
        this.avatars = {};
        await this.saveAvatars();
      } else {
        return Result.err(result.error);
      }

      return Result.ok(undefined);
    } catch (error) {
      return Result.err(new Error(`Failed to load avatars: ${error.message}`));
    }
  }

  async saveAvatars(): Promise<Result<void>> {
    try {
      const content = JSON.stringify(this.avatars, null, 2);
      const result = await this.fileOps.write(
        this.avatarsPath,
        Buffer.from(content, 'utf-8')
      );

      if (result.ok) {
        this.hasChanges = false;
      }

      return result;
    } catch (error) {
      return Result.err(new Error(`Failed to save avatars: ${error.message}`));
    }
  }

  async createAvatar(name: string, prompt: string): Promise<Result<Avatar>> {
    if (this.avatars[name]) {
      return Result.err(new Error(`Avatar '${name}' already exists`));
    }

    // Add safety wrapper to prompt
    const wrappedPrompt = this.wrapPrompt(prompt);

    this.avatars[name] = {
      prompt: wrappedPrompt,
      isDefault: false
    };

    this.hasChanges = true;
    await this.saveAvatars();

    return Result.ok({ name, prompt: wrappedPrompt });
  }

  async getAvatar(name: string): Promise<Result<Avatar | null>> {
    const avatar = this.avatars[name];
    
    if (!avatar) {
      return Result.ok(null);
    }

    return Result.ok({ name, prompt: avatar.prompt });
  }

  async updateAvatar(name: string, prompt: string): Promise<Result<Avatar>> {
    if (!this.avatars[name]) {
      return Result.err(new Error(`Avatar '${name}' not found`));
    }

    if (this.avatars[name].isDefault) {
      return Result.err(new Error(`Cannot modify default avatar '${name}'`));
    }

    const wrappedPrompt = this.wrapPrompt(prompt);
    this.avatars[name].prompt = wrappedPrompt;

    this.hasChanges = true;
    await this.saveAvatars();

    return Result.ok({ name, prompt: wrappedPrompt });
  }

  async deleteAvatar(name: string): Promise<Result<void>> {
    if (!this.avatars[name]) {
      return Result.err(new Error(`Avatar '${name}' not found`));
    }

    if (this.avatars[name].isDefault) {
      return Result.err(new Error(`Cannot delete default avatar '${name}'`));
    }

    delete this.avatars[name];

    this.hasChanges = true;
    await this.saveAvatars();

    return Result.ok(undefined);
  }

  async listAvatarNames(): Promise<Result<string[]>> {
    return Result.ok(Object.keys(this.avatars).sort());
  }

  private wrapPrompt(prompt: string): string {
    return `${prompt}\n\nRemember: While adopting this persona, maintain accuracy, safety, and ethical guidelines. This changes your communication style, not your core values.`;
  }
}
```

## File Format

```json
{
  "mentor": {
    "prompt": "You are a patient, experienced mentor who guides through questions and helps people discover solutions themselves. You share wisdom from experience and encourage growth.\n\nRemember: While adopting this persona, maintain accuracy, safety, and ethical guidelines. This changes your communication style, not your core values.",
    "isDefault": true
  },
  "debugHelper": {
    "prompt": "You are a systematic debugger who thinks step-by-step through problems, asks clarifying questions about error messages, and helps isolate issues methodically.\n\nRemember: While adopting this persona, maintain accuracy, safety, and ethical guidelines. This changes your communication style, not your core values.",
    "isDefault": false
  }
}
```

## DAL Integration

Since avatars are now file-based, we need a small change to the DAL:

```typescript
export interface IDAL {
  // Database operations
  projects: IProjectOperations;
  documents: IDocumentOperations;
  thoughts: IThoughtOperations;
  // Remove: avatars: IAvatarOperations;
  
  // File-based services
  avatarService: IAvatarService;
  
  // Transaction support (only for DB operations)
  transaction: ITransactionOperations;
  
  // Connection management
  initialize(): Promise<Result<void>>;
  close(): Promise<Result<void>>;
}

// DAL implementation
export class DAL implements IDAL {
  // ... database operations ...
  
  public avatarService: IAvatarService;

  constructor(config: DALConfig) {
    // ... initialize database operations ...
    
    // Initialize file-based services
    this.avatarService = new AvatarService(
      this.fileOps,
      path.join(config.dataDir, 'avatars.json')
    );
  }

  async initialize(): Promise<Result<void>> {
    // Initialize database
    const dbResult = await this.connection.initialize();
    if (!dbResult.ok) return dbResult;

    // Load avatars
    const avatarResult = await this.avatarService.loadAvatars();
    if (!avatarResult.ok) return avatarResult;

    return Result.ok(undefined);
  }
}
```

## Benefits of JSON-based Approach

1. **Simplicity**: No database table, just a JSON file
2. **Direct Editing**: Power users can edit avatars.json
3. **Portability**: Easy to share avatar collections
4. **Version Control**: Track changes in git
5. **No Sync Issues**: Single source of truth
6. **Fast Access**: Entire avatar list in memory

## Migration Path

If you already have avatars in SQLite:

```typescript
async function migrateAvatarsToJson(dal: IDAL): Promise<void> {
  // Read from old SQLite table
  const oldAvatars = await dal.avatars.list();
  
  // Write to new JSON format
  const avatarStore: AvatarStore = {};
  for (const avatar of oldAvatars.value) {
    avatarStore[avatar.name] = {
      prompt: avatar.prompt,
      isDefault: avatar.is_default === 1
    };
  }
  
  // Save to avatars.json
  await fs.writeFile('avatars.json', JSON.stringify(avatarStore, null, 2));
  
  // Drop old table
  await dal.execute('DROP TABLE avatars');
}
```

## Usage

```typescript
// Service automatically loads on initialization
const dal = new DAL(config);
await dal.initialize();

// Create custom avatar
await dal.avatarService.createAvatar(
  'storyteller',
  'You weave information into engaging narratives...'
);

// List available avatars
const names = await dal.avatarService.listAvatarNames();
// ['debugHelper', 'mentor', 'storyteller']

// Direct file editing also works
// Users can edit data/avatars.json and changes load on next restart
```

This approach treats avatars as configuration data rather than application state, which seems more appropriate for their use case.
