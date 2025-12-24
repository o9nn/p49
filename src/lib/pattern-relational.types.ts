/**
 * pattern-relational.types.ts
 * TypeScript types for Pattern Dynamics relational expressions and holarchies
 * Based on the PD49Table and holarchical structures from markdown documentation
 */

// ============================================================================
// I. Seven Core Patterns (First Order Patterns)
// ============================================================================

export type CorePattern = 
  | 'source'
  | 'dynamics'
  | 'creative'
  | 'exchange'
  | 'structure'
  | 'polarity'
  | 'rhythm';

export const CORE_PATTERNS: readonly CorePattern[] = [
  'source',
  'dynamics',
  'creative',
  'exchange',
  'structure',
  'polarity',
  'rhythm'
] as const;

// ============================================================================
// II. Set-Theoretic Operators (from PD49Table header)
// ============================================================================

export type SetOperator = 
  | '∈'   // IS - element of
  | '⊂'   // Dynamics - subset
  | '∩'   // OR - intersection
  | '⊆'   // Exchange - subset or equal
  | '∪'   // Structure - union
  | '∉'   // NOT - not element of
  | '∅';  // EMPTY - empty set

export interface SetTheoreticRelation {
  readonly operator: SetOperator;
  readonly semanticMeaning: string;
  readonly corePattern?: CorePattern;
}

export const SET_OPERATORS: Record<SetOperator, SetTheoreticRelation> = {
  '∈': { operator: '∈', semanticMeaning: 'IS', corePattern: undefined },
  '⊂': { operator: '⊂', semanticMeaning: 'Dynamics', corePattern: 'dynamics' },
  '∩': { operator: '∩', semanticMeaning: 'OR', corePattern: undefined },
  '⊆': { operator: '⊆', semanticMeaning: 'Exchange', corePattern: 'exchange' },
  '∪': { operator: '∪', semanticMeaning: 'Structure', corePattern: 'structure' },
  '∉': { operator: '∉', semanticMeaning: 'NOT', corePattern: undefined },
  '∅': { operator: '∅', semanticMeaning: 'EMPTY', corePattern: undefined }
} as const;

// ============================================================================
// III. Pattern Relationship Matrix (7x7 = 49 relationships)
// ============================================================================

export interface PatternRelationship {
  readonly from: CorePattern;
  readonly to: CorePattern;
  readonly relationship: string;
  readonly description: string;
  readonly cellNumber?: number; // Some cells have numbered references
}

// The 49-element relational matrix
export type PatternMatrix = Record<CorePattern, Record<CorePattern, PatternRelationship>>;

// ============================================================================
// IV. Holarchical Relationship Types
// ============================================================================

export type HolarchyRelationType = 
  | 'contains'        // Parent contains child
  | 'transcends'      // Higher level transcends lower
  | 'includes'        // Includes as part
  | 'emerges-from'    // Emerges from lower level
  | 'coordinates'     // Coordinates activities of
  | 'regulates'       // Regulates behavior of
  | 'composes';       // Composes to form higher level

export interface HolarchicalRelation {
  readonly type: HolarchyRelationType;
  readonly parent: CorePattern | string;
  readonly child: CorePattern | string;
  readonly level: number;  // Level in holarchy (0 = base)
  readonly bidirectional?: boolean;
  readonly description: string;
}

// ============================================================================
// V. Global Processes (from PD49Table)
// ============================================================================

export type GlobalProcessType = 
  | 'closure'         // Local/global control
  | 'folding'         // Global process of folding
  | 'unfolding'       // Global process of unfolding
  | 'distinction';    // Global process of distinction

export interface GlobalProcess {
  readonly type: GlobalProcessType;
  readonly scope: 'local' | 'global';
  readonly number: number;  // Reference number (e.g., 17, 23, 28, 29, 39, 40, 41, 42)
  readonly mechanism: string;
  readonly examples: readonly string[];
}

// ============================================================================
// VI. Pattern Aspects and Qualities
// ============================================================================

export interface PatternAspect {
  readonly pattern: CorePattern;
  readonly aspectName: string;
  readonly quality: string;
  readonly row: CorePattern;
  readonly column: CorePattern;
}

// Source pattern aspects (from PD49Table row 1)
export const SOURCE_ASPECTS = {
  source: { aspectName: 'Ground Source', quality: 'Unity Existence' },
  dynamics: { aspectName: 'Unity Dynamics', quality: 'Creative Grace' },
  creative: { aspectName: 'Creative Grace', quality: 'Source Stream' },
  exchange: { aspectName: 'Source Stream', quality: 'Resonance Structure' },
  structure: { aspectName: 'Resonance Structure', quality: 'High Polarity' },
  polarity: { aspectName: 'High Polarity', quality: 'Field Vibrations' },
  rhythm: { aspectName: 'Field Vibrations', quality: 'Unity Existence Perception' }
} as const;

// Dynamics pattern aspects (from PD49Table row 2)
export const DYNAMICS_ASPECTS = {
  source: { aspectName: 'Dynamic Order', quality: 'Integrated Dynamics' },
  dynamics: { aspectName: 'Integrated Dynamics', quality: 'Dynamic Creativity' },
  creative: { aspectName: 'Dynamic Creativity', quality: 'Operation Dynamics' },
  exchange: { aspectName: 'Operation Dynamics', quality: 'Structural Dynamics' },
  structure: { aspectName: 'Structural Dynamics', quality: 'Dynamic Polarity' },
  polarity: { aspectName: 'Dynamic Polarity', quality: 'Emergent Oppositions' },
  rhythm: { aspectName: 'Emergent Oppositions', quality: 'Dynamics' }
} as const;

// Creative pattern aspects (from PD49Table row 3)
export const CREATIVE_ASPECTS = {
  source: { aspectName: 'Self Creation', quality: 'Dynamic Response' },
  dynamics: { aspectName: 'Dynamic Response', quality: 'Innovative Arising' },
  creative: { aspectName: 'Innovative Arising', quality: 'Exchange Creation' },
  exchange: { aspectName: 'Exchange Creation', quality: 'Order Creation' },
  structure: { aspectName: 'Order Creation', quality: 'Creative Polarity' },
  polarity: { aspectName: 'Creative Polarity', quality: 'Mesh-Works' },
  rhythm: { aspectName: 'Mesh-Works', quality: 'Creative' }
} as const;

// Exchange pattern aspects (from PD49Table row 4)
export const EXCHANGE_ASPECTS = {
  source: { aspectName: 'Work Rate', quality: 'Adjustment Dynamics' },
  dynamics: { aspectName: 'Adjustment Dynamics', quality: 'Creative Prosperity' },
  creative: { aspectName: 'Creative Prosperity', quality: 'Essence of Exchange' },
  exchange: { aspectName: 'Essence of Exchange', quality: 'Relational Design' },
  structure: { aspectName: 'Relational Design', quality: 'Exchange States' },
  polarity: { aspectName: 'Exchange States', quality: 'Flow Surges' },
  rhythm: { aspectName: 'Flow Surges', quality: 'Exchange' }
} as const;

// Structure pattern aspects (from PD49Table row 5)
export const STRUCTURE_ASPECTS = {
  source: { aspectName: 'Order Accrual', quality: 'Dynamical Structures' },
  dynamics: { aspectName: 'Dynamical Structures', quality: 'Structural Adjustments' },
  creative: { aspectName: 'Structural Adjustments', quality: 'Flow Container' },
  exchange: { aspectName: 'Flow Container', quality: 'Essential Structure' },
  structure: { aspectName: 'Essential Structure', quality: 'Polarity Structure' },
  polarity: { aspectName: 'Polarity Structure', quality: 'Rhythm Structures' },
  rhythm: { aspectName: 'Rhythm Structures', quality: 'Structure' }
} as const;

// Polarity pattern aspects (from PD49Table row 6)
export const POLARITY_ASPECTS = {
  source: { aspectName: 'Source Distinctions', quality: 'Part / Whole Polarity' },
  dynamics: { aspectName: 'Part / Whole Polarity', quality: 'Liminal Creation' },
  creative: { aspectName: 'Liminal Creation', quality: 'Relational Duality' },
  exchange: { aspectName: 'Relational Duality', quality: 'Part / Whole' },
  structure: { aspectName: 'Part / Whole', quality: 'Primordial Duality' },
  polarity: { aspectName: 'Primordial Duality', quality: 'Iterative Extremes' },
  rhythm: { aspectName: 'Iterative Extremes', quality: 'Complementarity Distinction' }
} as const;

// Rhythm pattern aspects (from PD49Table row 7)
export const RHYTHM_ASPECTS = {
  source: { aspectName: 'Ground Transformations', quality: 'Dynamic Rhythm' },
  dynamics: { aspectName: 'Dynamic Rhythm', quality: 'Emergent Creation' },
  creative: { aspectName: 'Emergent Creation', quality: 'Exchange Phases' },
  exchange: { aspectName: 'Exchange Phases', quality: 'Edge Pattern' },
  structure: { aspectName: 'Edge Pattern', quality: 'Rhythmic Duality' },
  polarity: { aspectName: 'Rhythmic Duality', quality: 'Recurrent Order' },
  rhythm: { aspectName: 'Recurrent Order', quality: 'Rhythm' }
} as const;

// ============================================================================
// VII. Relational Expressions
// ============================================================================

export interface RelationalExpression {
  readonly expression: string;
  readonly leftPattern: CorePattern;
  readonly operator: string;
  readonly rightPattern: CorePattern;
  readonly result: PatternRelationship;
}

// ============================================================================
// VIII. Holarchy Levels
// ============================================================================

export interface HolarchyLevel {
  readonly level: number;
  readonly name: string;
  readonly patterns: readonly CorePattern[];
  readonly description: string;
  readonly emergentProperty?: string;
}

// Example: A simplified holarchy structure
export const PATTERN_HOLARCHY: readonly HolarchyLevel[] = [
  {
    level: 0,
    name: 'Source Foundation',
    patterns: ['source'],
    description: 'The nondual ground from which all patterns emerge',
    emergentProperty: 'Unity'
  },
  {
    level: 1,
    name: 'Dynamic Manifestation',
    patterns: ['dynamics', 'rhythm'],
    description: 'Temporal patterns of change and flow',
    emergentProperty: 'Movement'
  },
  {
    level: 2,
    name: 'Creative Polarity',
    patterns: ['creative', 'polarity'],
    description: 'Differentiation and distinction',
    emergentProperty: 'Diversity'
  },
  {
    level: 3,
    name: 'Structural Exchange',
    patterns: ['structure', 'exchange'],
    description: 'Organization and interaction',
    emergentProperty: 'Integration'
  }
] as const;

// ============================================================================
// IX. Factory Functions
// ============================================================================

export function createPatternRelationship(
  from: CorePattern,
  to: CorePattern,
  relationship: string,
  description: string,
  cellNumber?: number
): PatternRelationship {
  return {
    from,
    to,
    relationship,
    description,
    cellNumber
  };
}

export function createHolarchicalRelation(
  type: HolarchyRelationType,
  parent: CorePattern | string,
  child: CorePattern | string,
  level: number,
  description: string,
  bidirectional: boolean = false
): HolarchicalRelation {
  return {
    type,
    parent,
    child,
    level,
    description,
    bidirectional
  };
}

export function createGlobalProcess(
  type: GlobalProcessType,
  scope: 'local' | 'global',
  number: number,
  mechanism: string,
  examples: readonly string[]
): GlobalProcess {
  return {
    type,
    scope,
    number,
    mechanism,
    examples
  };
}

// ============================================================================
// X. Utility Functions
// ============================================================================

export function getPatternAspect(
  pattern: CorePattern,
  column: CorePattern
): PatternAspect | undefined {
  const aspectMaps = {
    source: SOURCE_ASPECTS,
    dynamics: DYNAMICS_ASPECTS,
    creative: CREATIVE_ASPECTS,
    exchange: EXCHANGE_ASPECTS,
    structure: STRUCTURE_ASPECTS,
    polarity: POLARITY_ASPECTS,
    rhythm: RHYTHM_ASPECTS
  };

  const aspectMap = aspectMaps[pattern];
  const aspect = aspectMap?.[column];

  if (!aspect) return undefined;

  return {
    pattern,
    aspectName: aspect.aspectName,
    quality: aspect.quality,
    row: pattern,
    column
  };
}

export function getAllPatternRelationships(pattern: CorePattern): readonly PatternAspect[] {
  return CORE_PATTERNS.map(col => getPatternAspect(pattern, col))
    .filter((a): a is PatternAspect => a !== undefined);
}

export function findRelationshipPath(
  from: CorePattern,
  to: CorePattern
): readonly CorePattern[] | undefined {
  // Simple direct path for now
  if (from === to) return [from];
  
  // Try to find a path through the holarchy
  const fromLevel = PATTERN_HOLARCHY.findIndex(h => h.patterns.includes(from));
  const toLevel = PATTERN_HOLARCHY.findIndex(h => h.patterns.includes(to));
  
  if (fromLevel === -1 || toLevel === -1) return undefined;
  
  // For now, just return the direct relationship
  return [from, to];
}

// ============================================================================
// XI. Relational Expression Evaluation
// ============================================================================

export function evaluateRelation(
  left: CorePattern,
  operator: SetOperator,
  right: CorePattern
): string {
  const relation = SET_OPERATORS[operator];
  const aspect = getPatternAspect(left, right);
  
  return `${left} ${operator} ${right} → ${aspect?.quality || 'unknown relationship'}`;
}

export function composePatterns(
  pattern1: CorePattern,
  pattern2: CorePattern
): PatternRelationship {
  const aspect = getPatternAspect(pattern1, pattern2);
  
  return createPatternRelationship(
    pattern1,
    pattern2,
    'composition',
    aspect ? `${aspect.aspectName}: ${aspect.quality}` : 'Unknown composition'
  );
}
