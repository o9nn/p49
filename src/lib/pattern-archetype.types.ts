/**
 * TypeScript type definitions mirroring the Scheme Pattern Archetype model
 * 
 * This file provides type-safe representations of the Integral Semiotic Realism
 * Pattern Primary Defining Archetype, enabling integration between the pure
 * Scheme implementation and the TypeScript/React visualization.
 */

// ============================================================================
// Core Perspective Types
// ============================================================================

export type Perspective = '1st-person' | '2nd-person' | '3rd-person';

export type Domain = 'epistemology' | 'ontology' | 'methodology';

export type Category = 'possibility' | 'existence' | 'mediation';

// ============================================================================
// Semiotic Categories
// ============================================================================

/**
 * Firstness: Quality, possibility, feeling, immediate presence
 * The Sign aspect - epistemic, subjective, 1st person perspective
 */
export interface Firstness {
  type: 'firstness';
  sign: string;
  quality: string;
  perspective: '1st-person';
  domain: 'epistemology';
  category: 'possibility';
}

/**
 * Secondness: Existence, actuality, brute fact, resistance
 * The Object aspect - ontological, objective, 3rd person perspective
 */
export interface Secondness {
  type: 'secondness';
  object: string;
  actuality: string;
  perspective: '3rd-person';
  domain: 'ontology';
  category: 'existence';
}

/**
 * Thirdness: Mediation, law, habit, continuity
 * The Interpretant aspect - methodological, intersubjective, 2nd person perspective
 */
export interface Thirdness {
  type: 'thirdness';
  interpretant: string;
  law: string;
  perspective: '2nd-person';
  domain: 'methodology';
  category: 'mediation';
}

export type SemioticCategory = Firstness | Secondness | Thirdness;

// ============================================================================
// NonDual Origin
// ============================================================================

export interface NonDualOrigin {
  type: 'nondual-origin';
  zone: 'subsistence';
  domain: 'intransitive';
  nature: 'undifferentiated';
  potentiality: 'infinite';
  description: string;
}

// ============================================================================
// Domains and Zones
// ============================================================================

export interface ActualDomain {
  type: 'actual-domain';
  nature: 'manifest';
  contents: ['epistemic-signs', 'ontological-objects'];
  description: string;
}

export interface IntransitiveDomain {
  type: 'intransitive-domain';
  nature: 'relational';
  zone: 'subsistence';
  contents: ['semiotic-processes', 'meaning-making'];
  description: string;
}

export interface EmpiricalDomain {
  type: 'empirical-domain';
  nature: 'experiential';
  zone: 'existence';
  contents: ['enactment', 'practice'];
  description: string;
}

export type DomainType = ActualDomain | IntransitiveDomain | EmpiricalDomain;

// ============================================================================
// Semiotic Processes
// ============================================================================

export interface ContinuousSignification {
  type: 'continuous-signification';
  process: 'method';
  from: 'firstness';
  through: 'thirdness';
  to: 'secondness';
  sign: string;
  interpretant: string;
  object: string;
  description: string;
}

export interface EpistemicEmergence {
  type: 'epistemic-emergence';
  from: 'nondual-origin';
  to: 'firstness';
  origin: NonDualOrigin;
  emergent: string;
  direction: 'subjective';
  description: string;
}

export interface OntologicalEmergence {
  type: 'ontological-emergence';
  from: 'nondual-origin';
  to: 'secondness';
  origin: NonDualOrigin;
  emergent: string;
  direction: 'objective';
  description: string;
}

export interface NonDualReturn {
  type: 'nondual-return';
  from: 'firstness' | 'secondness' | 'thirdness';
  to: 'nondual-origin';
  entity: SemioticCategory;
  process: 'evolution';
  description: string;
}

export type SemioticProcess =
  | ContinuousSignification
  | EpistemicEmergence
  | OntologicalEmergence
  | NonDualReturn;

// ============================================================================
// NonDual Evolution
// ============================================================================

export interface NonDualEvolution {
  type: 'nondual-evolution';
  leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'];
  rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'];
  description: string;
}

// ============================================================================
// Semiotic Cycle
// ============================================================================

export interface SemioticCycle {
  type: 'semiotic-cycle';
  nodes: [Firstness, Secondness, Thirdness];
  processes: SemioticProcess[];
  origin: NonDualOrigin;
  evolution: NonDualEvolution;
  domains: [ActualDomain, IntransitiveDomain, EmpiricalDomain];
}

// ============================================================================
// Perspectival System
// ============================================================================

export interface PerspectiveMapping {
  '1st-person': ['firstness', 'subject', 'epistemology'];
  '2nd-person': ['thirdness', 'interpretant', 'methodology'];
  '3rd-person': ['secondness', 'object', 'ontology'];
}

// ============================================================================
// Complex Perspectival Systems
// ============================================================================

export interface MetaType {
  type: 'meta-type';
  name: string;
  structure: string[];
  level: 'transcendent';
  description: string;
}

export interface CosmicHabit {
  type: 'cosmic-habit';
  name: string;
  tendency: string;
  temporality: 'evolutionary';
  description: string;
}

export interface DynamicPattern {
  type: 'dynamic-pattern';
  metaType: MetaType;
  cosmicHabit: CosmicHabit;
  integration: 'complex-perspectival-system';
}

// ============================================================================
// Integral Archetype - The Complete Pattern
// ============================================================================

export interface IntegralArchetype {
  type: 'integral-archetype';
  name: string;
  semioticCycle: SemioticCycle;
  metaPattern: DynamicPattern;
  nonDualOrigin: NonDualOrigin;
  perspectivalSystem: PerspectiveMapping;
  description: string;
}

// ============================================================================
// Factory Functions (matching the Scheme constructors)
// ============================================================================

export function makeFirstness(sign: string, quality: string): Firstness {
  return {
    type: 'firstness',
    sign,
    quality,
    perspective: '1st-person',
    domain: 'epistemology',
    category: 'possibility',
  };
}

export function makeSecondness(object: string, actuality: string): Secondness {
  return {
    type: 'secondness',
    object,
    actuality,
    perspective: '3rd-person',
    domain: 'ontology',
    category: 'existence',
  };
}

export function makeThirdness(interpretant: string, law: string): Thirdness {
  return {
    type: 'thirdness',
    interpretant,
    law,
    perspective: '2nd-person',
    domain: 'methodology',
    category: 'mediation',
  };
}

export function makeMetaType(name: string, structure: string[]): MetaType {
  return {
    type: 'meta-type',
    name,
    structure,
    level: 'transcendent',
    description: `A pattern structure that organizes multiple perspectives`,
  };
}

export function makeCosmicHabit(name: string, tendency: string): CosmicHabit {
  return {
    type: 'cosmic-habit',
    name,
    tendency,
    temporality: 'evolutionary',
    description: `A habit-like pattern that shapes cosmic evolution`,
  };
}

export function makeDynamicPattern(
  name: string,
  structure: string[],
  tendency: string
): DynamicPattern {
  return {
    type: 'dynamic-pattern',
    metaType: makeMetaType(name, structure),
    cosmicHabit: makeCosmicHabit(name, tendency),
    integration: 'complex-perspectival-system',
  };
}

// ============================================================================
// Constants (matching the Scheme definitions)
// ============================================================================

export const NONDUAL_ORIGIN: NonDualOrigin = {
  type: 'nondual-origin',
  zone: 'subsistence',
  domain: 'intransitive',
  nature: 'undifferentiated',
  potentiality: 'infinite',
  description:
    'The zone from which all semiotic processes emerge and to which they return',
};

export const ACTUAL_DOMAIN: ActualDomain = {
  type: 'actual-domain',
  nature: 'manifest',
  contents: ['epistemic-signs', 'ontological-objects'],
  description: 'Domain of actualized forms where signs and objects exist',
};

export const INTRANSITIVE_DOMAIN: IntransitiveDomain = {
  type: 'intransitive-domain',
  nature: 'relational',
  zone: 'subsistence',
  contents: ['semiotic-processes', 'meaning-making'],
  description: 'Realm of pure relationality before actualization',
};

export const EMPIRICAL_DOMAIN: EmpiricalDomain = {
  type: 'empirical-domain',
  nature: 'experiential',
  zone: 'existence',
  contents: ['enactment', 'practice'],
  description: 'Realm of lived experience and empirical manifestation',
};

export const PERSPECTIVE_MAP: PerspectiveMapping = {
  '1st-person': ['firstness', 'subject', 'epistemology'] as const,
  '2nd-person': ['thirdness', 'interpretant', 'methodology'] as const,
  '3rd-person': ['secondness', 'object', 'ontology'] as const,
} as const;

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Get the category associated with a perspective
 */
export function perspectiveToCategory(
  perspective: Perspective
): ['firstness' | 'secondness' | 'thirdness', string, Domain] {
  return PERSPECTIVE_MAP[perspective];
}

/**
 * Type guard to check if a value is a SemioticCategory
 */
export function isSemioticCategory(value: unknown): value is SemioticCategory {
  return (
    typeof value === 'object' &&
    value !== null &&
    'type' in value &&
    ((value as any).type === 'firstness' ||
      (value as any).type === 'secondness' ||
      (value as any).type === 'thirdness')
  );
}

/**
 * Type guard to check if a value is an IntegralArchetype
 */
export function isIntegralArchetype(value: unknown): value is IntegralArchetype {
  return (
    typeof value === 'object' &&
    value !== null &&
    'type' in value &&
    (value as any).type === 'integral-archetype'
  );
}
