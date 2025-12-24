/**
 * pattern-dynamics.types.ts
 * TypeScript types for Pattern Dynamics integration with Integral Semiotic Realism
 * Based on analysis of images in /patterns folder
 */

import type { 
  Firstness, 
  Secondness, 
  Thirdness, 
  IntegralArchetype,
  DynamicPattern,
  CosmicHabit,
  MetaType
} from './pattern-archetype.types';

// ============================================================================
// I. Six Evolutionary Cycles
// ============================================================================

export type EvolutionStage = 0 | 1 | 2 | 3 | 4 | 5 | 6;

export type PlaneName = 'physical' | 'potential' | 'holistic';

export interface EvolutionCycle {
  readonly stage: EvolutionStage;
  readonly name: string;
  readonly description: string;
  readonly planesActive: readonly PlaneName[];
  readonly characteristics: readonly string[];
}

// Stage 0: Eigen Drift
export interface EigenDrift extends EvolutionCycle {
  readonly stage: 0;
  readonly name: 'eigen-drift';
  readonly domain: 'nondual-origin';
  readonly planesActive: readonly [];
}

// Stage 1: Morphogenesis
export interface Morphogenesis extends EvolutionCycle {
  readonly stage: 1;
  readonly name: 'morphogenesis';
  readonly planesActive: readonly ['physical'];
  readonly process: 'differentiation';
}

// Stage 2: Vortex
export interface Vortex extends EvolutionCycle {
  readonly stage: 2;
  readonly name: 'vortex';
  readonly planesActive: readonly ['physical', 'potential'];
  readonly process: 'interaction';
  readonly components: readonly [string, string];
}

// Stage 3: Homeostasis
export interface Homeostasis extends EvolutionCycle {
  readonly stage: 3;
  readonly name: 'homeostasis';
  readonly planesActive: readonly ['physical', 'potential'];
  readonly process: 'retroaction';
  readonly cosmicHabit: CosmicHabit;
}

// Stage 4: Autopoiesis
export interface Autopoiesis extends EvolutionCycle {
  readonly stage: 4;
  readonly name: 'autopoiesis';
  readonly planesActive: readonly ['physical', 'potential', 'holistic'];
  readonly process: 'self-production';
}

// Stage 5: Self-Reference
export interface SelfReference extends EvolutionCycle {
  readonly stage: 5;
  readonly name: 'self-reference';
  readonly planesActive: readonly ['physical', 'potential', 'holistic'];
  readonly process: 'identity-formation';
  readonly metaType: MetaType;
}

// Stage 6: Autogenesis
export interface Autogenesis extends EvolutionCycle {
  readonly stage: 6;
  readonly name: 'autogenesis';
  readonly planesActive: readonly ['holistic'];
  readonly process: 'autonomous-whole';
  readonly returnToOrigin: true;
}

export type AnyCycle = 
  | EigenDrift 
  | Morphogenesis 
  | Vortex 
  | Homeostasis 
  | Autopoiesis 
  | SelfReference 
  | Autogenesis;

export interface SixCycleEvolution {
  readonly cycles: readonly [
    EigenDrift,
    Morphogenesis,
    Vortex,
    Homeostasis,
    Autopoiesis,
    SelfReference,
    Autogenesis
  ];
  readonly trajectory: readonly string[];
  readonly framework: 'pattern-dynamics';
}

// ============================================================================
// II. Three Planes / Three Domains
// ============================================================================

export interface Plane {
  readonly name: PlaneName;
  readonly correspondsTo: 'actual-domain' | 'intransitive-domain' | 'empirical-domain';
  readonly category: 'firstness' | 'secondness' | 'thirdness';
  readonly aspect: string;
  readonly description: string;
}

export interface ThreePlanes {
  readonly physicalPlane: Plane;
  readonly potentialPlane: Plane;
  readonly holisticPlane: Plane;
}

// ============================================================================
// III. Infinity Loop Structure
// ============================================================================

export interface InfinityLoop {
  readonly morphology: 'lemniscate';
  readonly upperLoop: string; // Epistemic dimension
  readonly lowerLoop: string; // Ontological dimension
  readonly crossingPoint: 'nondual-origin';
  readonly continuousFlow: true;
  readonly structure: 'triadic-semiosis';
}

export type PDPosition = 
  | 'top' 
  | 'right' 
  | 'bottom-right' 
  | 'bottom' 
  | 'left' 
  | 'top-left';

export interface PDaspect {
  readonly position: PDPosition;
  readonly category: 'firstness' | 'secondness' | 'thirdness';
  readonly actions: readonly string[];
}

export interface PDOperatingModel {
  readonly center: 'sense-source';
  readonly aspects: {
    readonly feelRhythms: PDaspect;
    readonly locatePerspectives: PDaspect;
    readonly outlineStructures: PDaspect;
    readonly coordinatePerspectives: PDaspect;
    readonly designDecisions: PDaspect;
    readonly governReflectively: PDaspect;
  };
  readonly infinitySymbols: readonly InfinityLoop[];
}

// ============================================================================
// IV. Viable Systems Triad
// ============================================================================

export interface ViableSystemFunction {
  readonly aspect: string;
  readonly category: 'firstness' | 'secondness' | 'thirdness';
  readonly description: string;
  readonly perspective: '1st-person' | '2nd-person' | '3rd-person';
}

export interface ViableSystemTriad {
  readonly operations: ViableSystemFunction; // Secondness
  readonly coordination: ViableSystemFunction; // Thirdness
  readonly intelligence: ViableSystemFunction; // Firstness
  readonly recursiveStructure: true;
}

// ============================================================================
// V. Multi-Scale Learning Loops
// ============================================================================

export type LearningScale = 'daily' | 'weekly' | 'monthly' | 'quarterly' | 'yearly';

export interface LearningLoop {
  readonly scale: LearningScale;
  readonly observe: Firstness;
  readonly act: Secondness;
  readonly learn: Thirdness;
  readonly cycle: IntegralArchetype;
}

export interface MultiScaleLearning {
  readonly micro: LearningLoop;
  readonly meso: LearningLoop;
  readonly macro: LearningLoop;
  readonly integration: 'nested-cycles';
  readonly pattern: DynamicPattern;
}

// ============================================================================
// VI. Temporal Patterns: Rhythm and Repetition
// ============================================================================

export interface Rhythm {
  readonly category: 'firstness';
  readonly quality: string;
  readonly period: number | string;
  readonly variation: string;
  readonly experiential: true;
  readonly description: string;
}

export interface Repetition {
  readonly category: 'secondness';
  readonly action: string;
  readonly count: number;
  readonly interval: number | string;
  readonly measurable: true;
  readonly description: string;
}

export interface TemporalPattern {
  readonly rhythm: Rhythm; // Firstness - quality
  readonly repetition: Repetition; // Secondness - quantity
  readonly habit: CosmicHabit; // Thirdness - meaning
  readonly semioticCycle: IntegralArchetype;
}

// ============================================================================
// VII. Meta-Morphosis and Pattern Transformation
// ============================================================================

export interface MetaMorphosis {
  readonly currentState: DynamicPattern;
  readonly tensions: readonly ['tropic-drift', 'alea'];
  readonly verticalChange: 'emergence';
  readonly horizontalStability: 'homeostasis';
  readonly transformation: unknown; // Result of nondual-evolution
  readonly outcome: 'new-pattern-state';
}

export interface PhaseTransition {
  readonly from: DynamicPattern;
  readonly to: DynamicPattern;
  readonly process: MetaMorphosis;
  readonly threshold: 'critical-point';
  readonly islandOfChange: true;
}

// ============================================================================
// VIII. Pattern Dynamics Archetypes
// ============================================================================

export interface OrganizationalChangeArchetype extends IntegralArchetype {
  readonly firstness: 'stakeholder-perspectives';
  readonly secondness: 'structural-changes';
  readonly thirdness: 'coordinated-transformation';
  readonly metaPattern: {
    readonly name: 'organizational-change';
    readonly structure: readonly ['six-cycles', 'viable-system', 'learning-loops'];
    readonly tendency: 'evolutionary-development';
  };
}

export interface LivingSystemArchetype extends IntegralArchetype {
  readonly firstness: 'environmental-sensing';
  readonly secondness: 'metabolic-processes';
  readonly thirdness: 'autopoietic-organization';
  readonly metaPattern: {
    readonly name: 'living-system';
    readonly structure: readonly ['morphogenesis', 'autopoiesis', 'autogenesis'];
    readonly tendency: 'life-sustaining';
  };
}

export interface ConsciousBeingArchetype extends IntegralArchetype {
  readonly firstness: 'phenomenal-experience';
  readonly secondness: 'neural-correlates';
  readonly thirdness: 'self-reference';
  readonly metaPattern: {
    readonly name: 'consciousness';
    readonly structure: readonly ['self-referential-loop', 'identity', 'autonomous-whole'];
    readonly tendency: 'increasing-integration';
  };
}

// ============================================================================
// IX. Visual Notation
// ============================================================================

export interface VisualNotation {
  readonly infinityLoop: '∞';
  readonly crossedCircles: '⊕';
  readonly connectedCircles: '◯→◯';
  readonly nestedLoops: '∞(∞)';
  readonly triad: '△';
  readonly cycle: '↻';
  readonly emergence: '↑';
  readonly return: '↓';
}

// ============================================================================
// X. Complete Pattern Dynamics Framework
// ============================================================================

export interface PatternDynamicsFramework {
  readonly sixCycles: SixCycleEvolution;
  readonly threePlanes: ThreePlanes;
  readonly operatingModel: PDOperatingModel;
  readonly viableSystem: ViableSystemTriad;
  readonly learningLoops: MultiScaleLearning;
  readonly temporalPatterns: {
    readonly rhythm: Rhythm;
    readonly repetition: Repetition;
    readonly pattern: TemporalPattern;
  };
  readonly transformation: {
    readonly metaMorphosis: MetaMorphosis;
    readonly phaseTransition: PhaseTransition;
  };
  readonly notation: VisualNotation;
}

// ============================================================================
// XI. Factory Functions
// ============================================================================

export function createEigenDrift(): EigenDrift {
  return {
    stage: 0,
    name: 'eigen-drift',
    description: 'Spontaneous state of being without relations',
    domain: 'nondual-origin',
    planesActive: [],
    characteristics: ['vacuity', 'chaos', 'undifferentiated-being', 'fluctuation']
  };
}

export function createMorphogenesis(): Morphogenesis {
  return {
    stage: 1,
    name: 'morphogenesis',
    description: 'Separation of ONE into TWO',
    planesActive: ['physical'],
    process: 'differentiation',
    characteristics: ['division', 'replication', 'differentiation', 'alterity']
  };
}

export function createVortex(componentA: string, componentB: string): Vortex {
  return {
    stage: 2,
    name: 'vortex',
    description: 'Physical interactions between components',
    planesActive: ['physical', 'potential'],
    process: 'interaction',
    components: [componentA, componentB],
    characteristics: ['energy-exchange', 'matter-flux', 'circular-relation']
  };
}

export function createHomeostasis(cosmicHabit: CosmicHabit): Homeostasis {
  return {
    stage: 3,
    name: 'homeostasis',
    description: 'Cybernetic control maintaining stability',
    planesActive: ['physical', 'potential'],
    process: 'retroaction',
    cosmicHabit,
    characteristics: ['feedback', 'retroaction', 'balance', 'equilibrium']
  };
}

export function createAutopoiesis(): Autopoiesis {
  return {
    stage: 4,
    name: 'autopoiesis',
    description: 'Mutual production of virtual network and physical structures',
    planesActive: ['physical', 'potential', 'holistic'],
    process: 'self-production',
    characteristics: ['self-production', 'self-maintenance', 'autonomy', 'organization']
  };
}

export function createSelfReference(metaType: MetaType): SelfReference {
  return {
    stage: 5,
    name: 'self-reference',
    description: 'Dialogue between self and image leading to identity',
    planesActive: ['physical', 'potential', 'holistic'],
    process: 'identity-formation',
    metaType,
    characteristics: ['self-knowledge', 'identity', 'unity', 'autonomy']
  };
}

export function createAutogenesis(): Autogenesis {
  return {
    stage: 6,
    name: 'autogenesis',
    description: 'Self-creation leading to autonomous whole',
    planesActive: ['holistic'],
    process: 'autonomous-whole',
    returnToOrigin: true,
    characteristics: ['autonomy', 'wholeness', 'unity', 'being', 'identity']
  };
}

export function createInfinityLoop(
  upperLoop: string,
  lowerLoop: string
): InfinityLoop {
  return {
    morphology: 'lemniscate',
    upperLoop,
    lowerLoop,
    crossingPoint: 'nondual-origin',
    continuousFlow: true,
    structure: 'triadic-semiosis'
  };
}

export function createViableSystemTriad(
  operations: string,
  coordination: string,
  intelligence: string
): ViableSystemTriad {
  return {
    operations: {
      aspect: operations,
      category: 'secondness',
      description: 'Doing, production, execution',
      perspective: '3rd-person'
    },
    coordination: {
      aspect: coordination,
      category: 'thirdness',
      description: 'Regulating, optimizing, mediating',
      perspective: '2nd-person'
    },
    intelligence: {
      aspect: intelligence,
      category: 'firstness',
      description: 'Sensing, adapting, learning',
      perspective: '1st-person'
    },
    recursiveStructure: true
  };
}

export function createLearningLoop(
  observation: string,
  action: string,
  interpretation: string,
  scale: LearningScale
): Omit<LearningLoop, 'observe' | 'act' | 'learn' | 'cycle'> {
  return {
    scale
  };
}

export function createRhythm(
  quality: string,
  period: number | string,
  variation: string
): Rhythm {
  return {
    category: 'firstness',
    quality,
    period,
    variation,
    experiential: true,
    description: 'Felt temporal pattern with qualitative variation'
  };
}

export function createRepetition(
  action: string,
  count: number,
  interval: number | string
): Repetition {
  return {
    category: 'secondness',
    action,
    count,
    interval,
    measurable: true,
    description: 'Counted iterative process'
  };
}

// ============================================================================
// XII. Utility Functions
// ============================================================================

export function getEvolutionStage(n: EvolutionStage): AnyCycle | null {
  switch (n) {
    case 0: return createEigenDrift();
    case 1: return createMorphogenesis();
    case 2: return createVortex('component-a', 'component-b');
    case 3: return createHomeostasis({ name: 'stability', tendency: 'homeostatic' } as CosmicHabit);
    case 4: return createAutopoiesis();
    case 5: return createSelfReference({ name: 'self-reference', structure: 'recursive' } as MetaType);
    case 6: return createAutogenesis();
    default: return null;
  }
}

export function isAutonomous(cycle: AnyCycle): cycle is Autogenesis {
  return cycle.stage === 6 && 'returnToOrigin' in cycle && cycle.returnToOrigin === true;
}

export function getActivePlanes(cycle: AnyCycle): readonly PlaneName[] {
  return cycle.planesActive;
}

export function evolveThroughStages(initialState: unknown): readonly AnyCycle[] {
  return [
    createEigenDrift(),
    createMorphogenesis(),
    createVortex('component-1', 'component-2'),
    createHomeostasis({ name: 'stability', tendency: 'equilibrium' } as CosmicHabit),
    createAutopoiesis(),
    createSelfReference({ name: 'identity', structure: 'self-referential' } as MetaType),
    createAutogenesis()
  ];
}
