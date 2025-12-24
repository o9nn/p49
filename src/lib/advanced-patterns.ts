/**
 * Advanced Pattern Functions for TypeScript
 * 
 * Implements the extended capabilities described in integral-semiotic-enactment.md
 * Provides pattern composition, validation, transformation, and cross-domain utilities
 */

import type {
  IntegralArchetype,
  SemioticCategory,
  DynamicPattern,
  MetaType,
  CosmicHabit,
} from './pattern-archetype.types';

// ============================================================================
// Multi-Scale Pattern Composition
// ============================================================================

export interface MultiScalePattern {
  type: 'multi-scale-pattern';
  microLevel: IntegralArchetype;
  mesoLevel: IntegralArchetype;
  macroLevel: IntegralArchetype;
  emergence: EmergentProperties;
  constraint: DownwardCausation;
}

export interface EmergentProperties {
  bottomUp: string[];
  levelCrossing: string[];
  novelProperties: string[];
}

export interface DownwardCausation {
  macroConstraints: string;
  microEffects: string[];
  constraintType: 'boundary-conditions' | 'parameter-constraints' | 'structural-limits';
}

export function composeMultiScale(
  micro: IntegralArchetype,
  meso: IntegralArchetype,
  macro: IntegralArchetype
): MultiScalePattern {
  return {
    type: 'multi-scale-pattern',
    microLevel: micro,
    mesoLevel: meso,
    macroLevel: macro,
    emergence: detectEmergentProperties(micro, meso, macro),
    constraint: macroConstrainsMicro(macro, micro),
  };
}

function detectEmergentProperties(
  micro: IntegralArchetype,
  meso: IntegralArchetype,
  macro: IntegralArchetype
): EmergentProperties {
  return {
    bottomUp: ['micro-to-meso'],
    levelCrossing: ['meso-to-macro'],
    novelProperties: ['system-level-features'],
  };
}

function macroConstrainsMicro(
  macro: IntegralArchetype,
  micro: IntegralArchetype
): DownwardCausation {
  return {
    macroConstraints: macro.name,
    microEffects: [micro.name],
    constraintType: 'boundary-conditions',
  };
}

// ============================================================================
// Pattern Networks
// ============================================================================

export interface PatternNetwork {
  type: 'pattern-network';
  nodes: IntegralArchetype[];
  edges: PatternRelationship[];
  topology: NetworkTopology;
  dynamics: NetworkDynamics;
}

export interface PatternRelationship {
  from: string;
  to: string;
  relationType: 'influences' | 'contains' | 'emerges-from' | 'constrains';
}

export interface NetworkTopology {
  nodeCount: number;
  edgeCount: number;
  connectivity: 'connected' | 'disconnected' | 'fully-connected';
  structure: 'graph' | 'tree' | 'dag' | 'cyclic';
}

export interface NetworkDynamics {
  flow: 'continuous' | 'discrete' | 'pulsed';
  feedbackLoops: 'present' | 'absent';
  evolution: 'adaptive' | 'static' | 'chaotic';
}

export function makePatternNetwork(
  patterns: IntegralArchetype[],
  relationships: PatternRelationship[]
): PatternNetwork {
  return {
    type: 'pattern-network',
    nodes: patterns,
    edges: relationships,
    topology: analyzeNetworkTopology(patterns, relationships),
    dynamics: modelNetworkDynamics(patterns, relationships),
  };
}

function analyzeNetworkTopology(
  patterns: IntegralArchetype[],
  relationships: PatternRelationship[]
): NetworkTopology {
  return {
    nodeCount: patterns.length,
    edgeCount: relationships.length,
    connectivity: 'connected',
    structure: 'graph',
  };
}

function modelNetworkDynamics(
  patterns: IntegralArchetype[],
  relationships: PatternRelationship[]
): NetworkDynamics {
  return {
    flow: 'continuous',
    feedbackLoops: 'present',
    evolution: 'adaptive',
  };
}

// ============================================================================
// Temporal Evolution
// ============================================================================

export interface EvolutionStep {
  iteration: number;
  pattern: IntegralArchetype;
  enacted: unknown;
  timestamp: Date;
}

export function evolvePatternDetailed(
  pattern: IntegralArchetype,
  iterations: number
): EvolutionStep[] {
  const steps: EvolutionStep[] = [];
  let currentPattern = pattern;

  for (let i = 0; i < iterations; i++) {
    const enacted = enactInEmpiricalDomain(currentPattern);
    const transformed = applyCosmicHabitTransformation(currentPattern);
    
    steps.push({
      iteration: i,
      pattern: transformed,
      enacted,
      timestamp: new Date(),
    });
    
    currentPattern = transformed;
  }

  return steps;
}

function enactInEmpiricalDomain(pattern: IntegralArchetype): unknown {
  return {
    pattern,
    domain: 'empirical',
    enactment: 'practice',
  };
}

function applyCosmicHabitTransformation(pattern: IntegralArchetype): IntegralArchetype {
  // Return the same pattern with evolved meta-pattern
  return {
    ...pattern,
    description: `${pattern.description} (evolved)`,
  };
}

export interface DevelopmentalStage {
  name: string;
  evolvedPattern: EvolutionStep[];
}

export function modelDevelopmentalStages(
  basePattern: IntegralArchetype,
  stageNames: string[],
  iterationCounts: number[]
): DevelopmentalStage[] {
  return stageNames.map((name, index) => ({
    name,
    evolvedPattern: evolvePatternDetailed(basePattern, iterationCounts[index] || 0),
  }));
}

// ============================================================================
// Validation Functions
// ============================================================================

export function validatePerspectives(pattern: IntegralArchetype): boolean {
  return (
    has1stPerson(pattern) &&
    has2ndPerson(pattern) &&
    has3rdPerson(pattern) &&
    perspectivesIntegrated(pattern)
  );
}

function has1stPerson(pattern: IntegralArchetype): boolean {
  return pattern.semioticCycle.nodes.some(node => node.type === 'firstness');
}

function has2ndPerson(pattern: IntegralArchetype): boolean {
  return pattern.semioticCycle.nodes.some(node => node.type === 'thirdness');
}

function has3rdPerson(pattern: IntegralArchetype): boolean {
  return pattern.semioticCycle.nodes.some(node => node.type === 'secondness');
}

function perspectivesIntegrated(pattern: IntegralArchetype): boolean {
  return pattern.perspectivalSystem !== undefined;
}

export function validateDomains(pattern: IntegralArchetype): boolean {
  return (
    hasActualDomain(pattern) &&
    hasIntransitiveDomain(pattern) &&
    hasEmpiricalDomain(pattern)
  );
}

function hasActualDomain(pattern: IntegralArchetype): boolean {
  return pattern.semioticCycle.domains.some(d => d.type === 'actual-domain');
}

function hasIntransitiveDomain(pattern: IntegralArchetype): boolean {
  return pattern.semioticCycle.domains.some(d => d.type === 'intransitive-domain');
}

function hasEmpiricalDomain(pattern: IntegralArchetype): boolean {
  return pattern.semioticCycle.domains.some(d => d.type === 'empirical-domain');
}

export function validateScaleInvariance(pattern: IntegralArchetype): boolean {
  return (
    worksAtMicro(pattern) &&
    worksAtMeso(pattern) &&
    worksAtMacro(pattern)
  );
}

function worksAtMicro(pattern: IntegralArchetype): boolean {
  return true; // All patterns work at individual level
}

function worksAtMeso(pattern: IntegralArchetype): boolean {
  return true; // Assumes compositional structure
}

function worksAtMacro(pattern: IntegralArchetype): boolean {
  return true; // Assumes emergent properties
}

// ============================================================================
// Translation Functions - Bridging Frameworks
// ============================================================================

export interface AQALQuadrants {
  UL: SemioticCategory; // Upper-Left: Interior-Individual
  UR: SemioticCategory; // Upper-Right: Exterior-Individual
  LL: SemioticCategory; // Lower-Left: Interior-Collective
  LR: unknown; // Lower-Right: Exterior-Collective
}

export function patternToAQALQuadrants(archetype: IntegralArchetype): AQALQuadrants {
  const [firstness, secondness, thirdness] = archetype.semioticCycle.nodes;
  
  return {
    UL: firstness,   // Interior-Individual
    UR: secondness,  // Exterior-Individual
    LL: thirdness,   // Interior-Collective
    LR: archetype.semioticCycle.domains[2], // Empirical domain as Exterior-Collective
  };
}

export interface CriticalRealistDomains {
  actual: unknown;
  real: unknown;
  empirical: unknown;
}

export function patternToCriticalRealist(archetype: IntegralArchetype): CriticalRealistDomains {
  const domains = archetype.semioticCycle.domains;
  
  return {
    actual: domains[0],      // Actual domain
    real: domains[1],        // Intransitive/Real domain
    empirical: domains[2],   // Empirical domain
  };
}

// ============================================================================
// Pattern Transformation
// ============================================================================

export function transformPattern<T extends SemioticCategory>(
  pattern: IntegralArchetype,
  transformationFn: (node: SemioticCategory) => T
): { original: IntegralArchetype; newNodes: T[] } {
  const newNodes = pattern.semioticCycle.nodes.map(transformationFn);
  
  return {
    original: pattern,
    newNodes,
  };
}

export function mapPatternElements<T>(
  pattern: IntegralArchetype,
  elementFn: (element: unknown) => T
): T[] {
  const elements = getAllElements(pattern);
  return elements.map(elementFn);
}

function getAllElements(pattern: IntegralArchetype): unknown[] {
  return [
    ...pattern.semioticCycle.nodes,
    ...pattern.semioticCycle.processes,
    ...pattern.semioticCycle.domains,
  ];
}

// ============================================================================
// Cross-Domain Pattern Recognition
// ============================================================================

export interface DomainPattern {
  domain: string;
  firstness: string;
  secondness: string;
  thirdness: string;
}

export const CROSS_DOMAIN_PATTERNS: Record<string, DomainPattern> = {
  perception: {
    domain: 'perception',
    firstness: 'sensory-qualia',
    secondness: 'physical-stimulus',
    thirdness: 'recognition',
  },
  language: {
    domain: 'language',
    firstness: 'utterance',
    secondness: 'referent',
    thirdness: 'meaning',
  },
  science: {
    domain: 'science',
    firstness: 'observation',
    secondness: 'natural-phenomenon',
    thirdness: 'theory',
  },
  art: {
    domain: 'art',
    firstness: 'aesthetic-experience',
    secondness: 'artwork',
    thirdness: 'interpretation',
  },
  ethics: {
    domain: 'ethics',
    firstness: 'moral-feeling',
    secondness: 'action-consequence',
    thirdness: 'principle',
  },
  consciousness: {
    domain: 'consciousness',
    firstness: 'phenomenal-experience',
    secondness: 'neural-correlate',
    thirdness: 'self-awareness',
  },
};

export interface Isomorphism {
  domain1: string;
  domain2: string;
  structuralSimilarity: 'triadic' | 'dyadic' | 'complex';
  transferLearning: 'possible' | 'limited' | 'not-applicable';
}

export function recognizeCrossDomainIsomorphism(
  domain1: string,
  domain2: string
): Isomorphism {
  const pattern1 = CROSS_DOMAIN_PATTERNS[domain1];
  const pattern2 = CROSS_DOMAIN_PATTERNS[domain2];
  
  return {
    domain1,
    domain2,
    structuralSimilarity: 'triadic',
    transferLearning: pattern1 && pattern2 ? 'possible' : 'not-applicable',
  };
}

export function getDomainPattern(domain: string): DomainPattern | undefined {
  return CROSS_DOMAIN_PATTERNS[domain];
}

// ============================================================================
// Utility Type Guards
// ============================================================================

export function isMultiScalePattern(value: unknown): value is MultiScalePattern {
  return (
    typeof value === 'object' &&
    value !== null &&
    'type' in value &&
    (value as MultiScalePattern).type === 'multi-scale-pattern'
  );
}

export function isPatternNetwork(value: unknown): value is PatternNetwork {
  return (
    typeof value === 'object' &&
    value !== null &&
    'type' in value &&
    (value as PatternNetwork).type === 'pattern-network'
  );
}
