# Pattern Archetype Integration Guide

This document describes how the pure Scheme implementation of the Pattern Primary Defining Archetype integrates with the TypeScript/React visualization.

## Architecture Overview

The project uses a dual-implementation approach:

1. **Theoretical Foundation (Scheme)**: Pure functional implementation in `scheme/` directory
2. **Visualization Layer (TypeScript/React)**: Interactive UI in `src/` directory

## File Mapping

### Scheme Implementation → TypeScript Types

| Scheme File | TypeScript File | Purpose |
|-------------|----------------|---------|
| `scheme/pattern-archetype.scm` | `src/lib/pattern-archetype.types.ts` | Core type definitions |
| (data structures) | `src/lib/pattern-archetype.data.ts` | Data provider for UI |
| (example archetype) | `src/components/InfinityLoop.tsx` | Visual representation |

## Type System Correspondence

### Semiotic Categories

**Scheme:**
```scheme
(define (make-firstness sign quality)
  `(firstness
    (sign . ,sign)
    (quality . ,quality)
    (perspective . 1st-person)
    (domain . epistemology)
    (category . possibility)))
```

**TypeScript:**
```typescript
export interface Firstness {
  type: 'firstness';
  sign: string;
  quality: string;
  perspective: '1st-person';
  domain: 'epistemology';
  category: 'possibility';
}
```

### Factory Functions

Both implementations provide corresponding factory functions:

**Scheme:**
```scheme
(make-firstness 'phenomenal-experience 'immediate-quality)
```

**TypeScript:**
```typescript
makeFirstness('phenomenal-experience', 'immediate-quality')
```

## Data Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    Scheme Implementation                     │
│                  (Theoretical Foundation)                    │
│                                                              │
│  pattern-archetype.scm                                      │
│  ├─ Core categories (Firstness, Secondness, Thirdness)     │
│  ├─ Semiotic processes                                      │
│  ├─ Domains and zones                                       │
│  └─ Example archetype                                       │
│                                                              │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       │ Mirrors structure
                       ↓
┌─────────────────────────────────────────────────────────────┐
│                  TypeScript Type System                      │
│                                                              │
│  pattern-archetype.types.ts                                 │
│  ├─ Interface definitions                                   │
│  ├─ Factory functions                                       │
│  ├─ Type guards                                             │
│  └─ Constants                                               │
│                                                              │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       │ Provides data
                       ↓
┌─────────────────────────────────────────────────────────────┐
│                    Data Provider Layer                       │
│                                                              │
│  pattern-archetype.data.ts                                  │
│  ├─ createExampleArchetype()                                │
│  ├─ getArchetypeNodes()                                     │
│  ├─ getArchetypePaths()                                     │
│  └─ getArchetypeDomains()                                   │
│                                                              │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       │ Consumed by
                       ↓
┌─────────────────────────────────────────────────────────────┐
│                  React Components (UI)                       │
│                                                              │
│  components/InfinityLoop.tsx                                │
│  ├─ SVG visualization of the archetype                      │
│  ├─ Interactive nodes (Firstness, Secondness, Thirdness)   │
│  └─ Animated paths (semiotic processes)                     │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Usage Examples

### 1. Using the Scheme Implementation

```scheme
;; Load the module
(load "scheme/pattern-archetype.scm")

;; Create an archetype
(define my-archetype
  (make-integral-archetype
    'experience
    'reality
    'knowledge
    (make-dynamic-pattern
      'cognition
      '(mental-model)
      'learning)))

;; Display it
(display-archetype my-archetype)
```

### 2. Using the TypeScript Implementation

```typescript
import { 
  createExampleArchetype, 
  getArchetypeNodes 
} from '@/lib/pattern-archetype.data';

// Get the archetype
const archetype = createExampleArchetype();

// Get nodes for visualization
const nodes = getArchetypeNodes(archetype);

// Use in React component
nodes.forEach(node => {
  console.log(node.label, node.description);
});
```

### 3. Creating Custom Instances

**Scheme:**
```scheme
(define perception-archetype
  (make-integral-archetype
    'sensory-input
    'physical-world
    'perceptual-understanding
    (make-dynamic-pattern
      'perception-pattern
      '(sensorimotor-loop)
      'adaptive-learning)))
```

**TypeScript:**
```typescript
import { 
  makeFirstness, 
  makeSecondness, 
  makeThirdness,
  makeDynamicPattern 
} from '@/lib/pattern-archetype.types';

const firstness = makeFirstness('sensory-input', 'immediate-quality');
const secondness = makeSecondness('physical-world', 'brute-existence');
const thirdness = makeThirdness('perceptual-understanding', 'mediating-law');
const pattern = makeDynamicPattern(
  'perception-pattern',
  ['sensorimotor-loop'],
  'adaptive-learning'
);
```

## Extending the System

When adding new features, maintain correspondence between implementations:

### Adding a New Category

1. **Scheme** (`scheme/pattern-archetype.scm`):
   ```scheme
   (define (make-new-category param1 param2)
     `(new-category
       (param1 . ,param1)
       (param2 . ,param2)))
   ```

2. **TypeScript** (`src/lib/pattern-archetype.types.ts`):
   ```typescript
   export interface NewCategory {
     type: 'new-category';
     param1: string;
     param2: string;
   }
   
   export function makeNewCategory(
     param1: string, 
     param2: string
   ): NewCategory {
     return { type: 'new-category', param1, param2 };
   }
   ```

3. **Data Provider** (`src/lib/pattern-archetype.data.ts`):
   ```typescript
   export function getNewCategoryData(category: NewCategory) {
     // Transform for UI consumption
   }
   ```

## Testing

### Scheme Tests

```bash
cd scheme
guile test-pattern.scm
# Or load and run:
# guile -l pattern-archetype.scm -l test-pattern.scm -c "(run-all-tests)"
```

### TypeScript Tests

The TypeScript types are validated at build time:

```bash
npm run build  # Type checking happens during build
npm run lint   # Additional linting
```

## Theoretical Foundations

Both implementations model the same theoretical framework:

### Peircean Triadic Semiotics
- **Firstness**: Sign (representamen)
- **Secondness**: Object (referent)
- **Thirdness**: Interpretant (meaning)

### Critical Realist Ontology
- **Actual Domain**: Manifest events
- **Intransitive Domain**: Structures and mechanisms
- **Empirical Domain**: Experienced events

### Nondual Philosophy
- **NonDual Origin**: Undifferentiated source
- **NonDual Evolution**: Cycle of emergence and return

## Benefits of Dual Implementation

1. **Formal Verification**: Scheme provides a pure, mathematical model
2. **Type Safety**: TypeScript ensures runtime correctness
3. **Educational**: Shows same concepts in different paradigms
4. **Documentation**: Each implementation documents the other
5. **Flexibility**: Can update UI without changing theory, and vice versa

## References

- **Scheme Implementation**: See `scheme/README.md`
- **TypeScript Types**: See `src/lib/pattern-archetype.types.ts`
- **React Visualization**: See `src/components/InfinityLoop.tsx`
- **Theoretical Framework**: See attached documents in issue

## Future Enhancements

Potential areas for integration:

1. **Code Generation**: Generate TypeScript types from Scheme definitions
2. **Runtime Validation**: Use Scheme as a validation oracle
3. **Property Testing**: Generate test cases from Scheme invariants
4. **Documentation**: Auto-generate docs from Scheme docstrings
5. **REPL Integration**: Embed Scheme interpreter in browser for live exploration
