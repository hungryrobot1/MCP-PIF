import { randomUUID } from 'crypto';

export interface ThoughtUnit {
    id: string;
    content: string;
    relationships: ThoughtRelation[];
}

export interface ThoughtRelation {
    type: 'sequence' | 'reflection' | 'association';
    targetId: string;
}

export class Thought {
    constructor(private unit: ThoughtUnit) {}

    static of(content: string): Thought {
        return new Thought({
            id: randomUUID(),
            content,
            relationships: []
        });
    }

    getId(): string {
        return this.unit.id;
    }

    getContent(): string {
        return this.unit.content;
    }

    getRelationships(): ThoughtRelation[] {
        return this.unit.relationships;
    }

    addRelation(type: ThoughtRelation['type'], targetId: string): Thought {
        this.unit.relationships.push({ type, targetId });
        return this;
    }
}