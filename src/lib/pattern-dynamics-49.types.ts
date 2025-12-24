/**
 * TypeScript type definitions for the 49 Pattern Dynamics patterns
 * 
 * This implements the complete Pattern Dynamics holarchy:
 * - 1 Zeroth-order pattern (Source with 7 aspects)
 * - 6 First-order patterns (Rhythm, Polarity, Structure, Exchange, Creativity, Dynamics)
 * - 42 Second-order patterns (7 major aspects × 6 minor aspects)
 * 
 * Total: 49 patterns
 * 
 * Integrated with Integral Semiotic Realism framework
 */

import type { Firstness, Secondness, Thirdness, NonDualOrigin } from './pattern-archetype.types';

// ============================================================================
// Pattern Order Types
// ============================================================================

export type PatternOrder = 0 | 1 | 2;

export type FirstOrderPatternName = 
  | 'rhythm' 
  | 'polarity' 
  | 'structure' 
  | 'exchange' 
  | 'creativity' 
  | 'dynamics';

export type MajorAspect = 'source' | FirstOrderPatternName;

// ============================================================================
// Source Pattern Aspects (7 aspects within Source)
// ============================================================================

export interface SourceAspect {
  name: string;
  altName: string;
  description: string;
  role: string;
  principle: string;
}

export interface SourcePattern {
  type: 'zeroth-order-pattern';
  name: 'source';
  order: 0;
  description: string;
  semioticStructure: {
    firstness: Firstness;
    secondness: Secondness;
    thirdness: Thirdness;
  };
  aspects: {
    sourceSource: SourceAspect;
    sourceDynamics: SourceAspect;
    sourceCreativity: SourceAspect;
    sourceExchange: SourceAspect;
    sourceStructure: SourceAspect;
    sourcePolarity: SourceAspect;
    sourceRhythm: SourceAspect;
  };
}

// ============================================================================
// First-Order Patterns
// ============================================================================

export type PatternDomain = 
  | 'temporality' 
  | 'differentiation' 
  | 'spatiality' 
  | 'relationality' 
  | 'generativity' 
  | 'processuality';

export interface FirstOrderPattern {
  type: 'first-order-pattern';
  name: FirstOrderPatternName;
  order: 1;
  description: string;
  firstness: Firstness;
  secondness: Secondness;
  thirdness: Thirdness;
  domain: PatternDomain;
  perspectiveIntegration: {
    firstPerson: string;
    secondPerson: string;
    thirdPerson: string;
  };
  principle: string;
}

// ============================================================================
// Second-Order Patterns
// ============================================================================

export interface HolarchicalPosition {
  major: MajorAspect;
  minor: FirstOrderPatternName;
}

export interface SecondOrderPattern {
  type: 'second-order-pattern';
  majorAspect: MajorAspect;
  minorAspect: FirstOrderPatternName;
  name: string;
  altName: string;
  order: 2;
  description: string;
  firstness: Firstness;
  secondness: Secondness;
  thirdness: Thirdness;
  role: string;
  principle: string;
  holarchicalPosition: HolarchicalPosition;
}

// ============================================================================
// Pattern Families (Groups of Second-Order Patterns)
// ============================================================================

export interface PatternFamily {
  familyName: string;
  majorAspect: MajorAspect;
  patterns: SecondOrderPattern[];
  count: number;
}

// ============================================================================
// Complete 49-Pattern Holarchy
// ============================================================================

export interface PatternHolarchyStructure {
  zerothOrder: 1;
  firstOrder: 6;
  secondOrder: 42;
}

export interface PatternHolarchy {
  type: 'pattern-holarchy';
  totalPatterns: 49;
  structure: PatternHolarchyStructure;
  
  // Zeroth-order (1 pattern)
  zerothOrderPatterns: [SourcePattern];
  
  // First-order (6 patterns)
  firstOrderPatterns: FirstOrderPattern[];
  
  // Second-order (42 patterns grouped by family)
  secondOrderPatterns: {
    sourceFamily: SecondOrderPattern[];      // 6 patterns
    dynamicsFamily: SecondOrderPattern[];    // 6 patterns
    creativityFamily: SecondOrderPattern[];  // 6 patterns
    exchangeFamily: SecondOrderPattern[];    // 6 patterns
    structureFamily: SecondOrderPattern[];   // 6 patterns
    polarityFamily: SecondOrderPattern[];    // 6 patterns
    rhythmFamily: SecondOrderPattern[];      // 6 patterns
  };
  
  nonDualOrigin: NonDualOrigin;
  framework: 'integral-semiotic-realism';
  integration: 'pattern-dynamics';
}

// ============================================================================
// Pattern Union Types
// ============================================================================

export type Pattern = SourcePattern | FirstOrderPattern | SecondOrderPattern;

export type AnyPattern = Pattern;

// ============================================================================
// Utility Types for Pattern Access
// ============================================================================

/**
 * Get patterns by order
 */
export interface PatternsByOrder {
  0: SourcePattern[];
  1: FirstOrderPattern[];
  2: SecondOrderPattern[];
}

/**
 * Pattern metadata for display and navigation
 */
export interface PatternMetadata {
  name: string;
  order: PatternOrder;
  description: string;
  principle?: string;
  role?: string;
}

/**
 * Pattern relationship tracking
 */
export interface PatternRelationship {
  from: string;
  to: string;
  relationshipType: 'generates' | 'crosses-with' | 'emerges-from' | 'returns-to';
  description?: string;
}

// ============================================================================
// Second-Order Pattern Names (by family)
// ============================================================================

export const SOURCE_FAMILY_PATTERNS = [
  'energy',
  'pattern',
  'power',
  'transformity',
  'resource',
  'autopoiesis',
] as const;

export const DYNAMICS_FAMILY_PATTERNS = [
  'system',
  'spontaneity',
  'feedback',
  'synergy',
  'agency-communion',
  'iterate',
] as const;

export const CREATIVITY_FAMILY_PATTERNS = [
  'evolution',
  'emergence',
  'growth',
  'adaptation',
  'bifurcation',
  'seed',
] as const;

export const EXCHANGE_FAMILY_PATTERNS = [
  'process',
  'uniqueness',
  'trade',
  'capture',
  'balance',
  'cycle',
] as const;

export const STRUCTURE_FAMILY_PATTERNS = [
  'holarchy',
  'complexity',
  'network',
  'hierarchy',
  'holon',
  'boundary',
] as const;

export const POLARITY_FAMILY_PATTERNS = [
  'competition-cooperation',
  'order-chaos',
  'flows-stores',
  'input-output',
  'concentration-diffusion',
  'expand-contract',
] as const;

export const RHYTHM_FAMILY_PATTERNS = [
  'enantiodromia',
  'synchronization',
  'pulse',
  'cadence',
  'swing',
  'repetition',
] as const;

export type SourceFamilyPattern = typeof SOURCE_FAMILY_PATTERNS[number];
export type DynamicsFamilyPattern = typeof DYNAMICS_FAMILY_PATTERNS[number];
export type CreativityFamilyPattern = typeof CREATIVITY_FAMILY_PATTERNS[number];
export type ExchangeFamilyPattern = typeof EXCHANGE_FAMILY_PATTERNS[number];
export type StructureFamilyPattern = typeof STRUCTURE_FAMILY_PATTERNS[number];
export type PolarityFamilyPattern = typeof POLARITY_FAMILY_PATTERNS[number];
export type RhythmFamilyPattern = typeof RHYTHM_FAMILY_PATTERNS[number];

export type SecondOrderPatternName = 
  | SourceFamilyPattern
  | DynamicsFamilyPattern
  | CreativityFamilyPattern
  | ExchangeFamilyPattern
  | StructureFamilyPattern
  | PolarityFamilyPattern
  | RhythmFamilyPattern;

// ============================================================================
// Factory Function Types
// ============================================================================

export type MakeSourcePattern = () => SourcePattern;
export type MakeFirstOrderPattern = (name: FirstOrderPatternName) => FirstOrderPattern;
export type MakeSecondOrderPattern = (
  major: MajorAspect,
  minor: FirstOrderPatternName,
  name: string,
  altName: string,
  description: string,
  role: string,
  principle: string
) => SecondOrderPattern;
export type MakePatternHolarchy = () => PatternHolarchy;

// ============================================================================
// Pattern Navigation and Query Types
// ============================================================================

export interface PatternQuery {
  order?: PatternOrder;
  majorAspect?: MajorAspect;
  minorAspect?: FirstOrderPatternName;
  name?: string;
  searchTerm?: string;
}

export interface PatternSearchResult {
  pattern: Pattern;
  relevance: number;
  matchedFields: string[];
}

// ============================================================================
// Visualization Types
// ============================================================================

export interface PatternNode {
  id: string;
  name: string;
  order: PatternOrder;
  type: 'source' | 'first-order' | 'second-order';
  description: string;
  family?: string;
}

export interface PatternEdge {
  source: string;
  target: string;
  relationship: PatternRelationship['relationshipType'];
  label?: string;
}

export interface PatternGraph {
  nodes: PatternNode[];
  edges: PatternEdge[];
}

// ============================================================================
// Export All Types
// ============================================================================

export type {
  SourcePattern,
  FirstOrderPattern,
  SecondOrderPattern,
  PatternFamily,
  PatternHolarchy,
  PatternHolarchyStructure,
  Pattern,
  PatternMetadata,
  PatternRelationship,
  HolarchicalPosition,
};
