/**
 * Data provider for the 49 Pattern Dynamics patterns
 * 
 * This module provides the complete implementation of all 49 patterns
 * with full Integral Semiotic Realism integration.
 */

import type {
  SourcePattern,
  FirstOrderPattern,
  SecondOrderPattern,
  PatternHolarchy,
  PatternNode,
  PatternEdge,
  PatternGraph,
  FirstOrderPatternName,
  MajorAspect,
} from './pattern-dynamics-49.types';

import {
  makeFirstness,
  makeSecondness,
  makeThirdness,
  NONDUAL_ORIGIN as nonDualOrigin,
} from './pattern-archetype.types';

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Create a second-order pattern
 */
function makeSecondOrderPattern(
  major: MajorAspect,
  minor: FirstOrderPatternName,
  name: string,
  altName: string,
  description: string,
  role: string,
  principle: string
): SecondOrderPattern {
  return {
    type: 'second-order-pattern',
    majorAspect: major,
    minorAspect: minor,
    name,
    altName,
    order: 2,
    description,
    firstness: makeFirstness(name, 'specific-quality'),
    secondness: makeSecondness(name, 'specific-actuality'),
    thirdness: makeThirdness(name, 'specific-law'),
    role,
    principle,
    holarchicalPosition: { major, minor },
  };
}

// ============================================================================
// Zeroth-Order Pattern: SOURCE
// ============================================================================

export const sourcePattern: SourcePattern = {
  type: 'zeroth-order-pattern',
  name: 'source',
  order: 0,
  description: 'The foundational ground from which all patterns emerge',
  semioticStructure: {
    firstness: makeFirstness('ground-of-being', 'pure-potentiality'),
    secondness: makeSecondness('undifferentiated-reality', 'infinite-actuality'),
    thirdness: makeThirdness('creative-principle', 'universal-law'),
  },
  aspects: {
    sourceSource: {
      name: 'void',
      altName: 'ground-source',
      description: 'The foundational ground of universal matter/energy consciousness',
      role: 'Signify consciousness necessary for order',
      principle: 'Conscious order',
    },
    sourceDynamics: {
      name: 'energy',
      altName: 'ground-transformations',
      description: 'Fundamental animating force of reality',
      role: 'Drive dynamic change and transformation',
      principle: 'Dynamic transformation',
    },
    sourceCreativity: {
      name: 'pattern',
      altName: 'dynamic-order',
      description: 'Repeated types of order',
      role: 'Provide successful templates of systemic design',
      principle: 'Potential and restrictiveness of form',
    },
    sourceExchange: {
      name: 'power',
      altName: 'work-rate',
      description: 'Productivity per unit time',
      role: 'Provide systemic productivity',
      principle: 'Productivity rates',
    },
    sourceStructure: {
      name: 'transformity',
      altName: 'order-accrual',
      description: 'Qualitative complexification',
      role: 'Bring advantages of structural increases in order',
      principle: 'Complexity creation',
    },
    sourcePolarity: {
      name: 'resource',
      altName: 'source-distinctions',
      description: 'Process of transformation of matter/energy',
      role: 'Provide different qualities of matter/energy',
      principle: 'Source transformations',
    },
    sourceRhythm: {
      name: 'autopoiesis',
      altName: 'self-creation',
      description: 'Ongoing self-authoring',
      role: 'Maintain unique system through creative growth',
      principle: 'Creative existence',
    },
  },
};

// ============================================================================
// First-Order Patterns (6 patterns)
// ============================================================================

export const rhythmPattern: FirstOrderPattern = {
  type: 'first-order-pattern',
  name: 'rhythm',
  order: 1,
  description: 'Temporal patterns, cycles, and repetition in time and space',
  firstness: makeFirstness('temporal-quality', 'rhythmic-feeling'),
  secondness: makeSecondness('cyclical-existence', 'actual-repetition'),
  thirdness: makeThirdness('rhythmic-law', 'periodic-habit'),
  domain: 'temporality',
  perspectiveIntegration: {
    firstPerson: '1st-person-time',
    secondPerson: '2nd-person-cadence',
    thirdPerson: '3rd-person-periodicity',
  },
  principle: 'Temporal organization through recurrence',
};

export const polarityPattern: FirstOrderPattern = {
  type: 'first-order-pattern',
  name: 'polarity',
  order: 1,
  description: 'Fundamental dualities and complementary oppositions',
  firstness: makeFirstness('polar-quality', 'dualistic-feeling'),
  secondness: makeSecondness('oppositional-existence', 'actual-duality'),
  thirdness: makeThirdness('complementary-law', 'dialectical-habit'),
  domain: 'differentiation',
  perspectiveIntegration: {
    firstPerson: '1st-person-distinction',
    secondPerson: '2nd-person-relation',
    thirdPerson: '3rd-person-opposition',
  },
  principle: 'Creative tension through complementary opposition',
};

export const structurePattern: FirstOrderPattern = {
  type: 'first-order-pattern',
  name: 'structure',
  order: 1,
  description: 'Organizational forms, boundaries, and spatial arrangements',
  firstness: makeFirstness('structural-quality', 'formal-feeling'),
  secondness: makeSecondness('organized-existence', 'actual-form'),
  thirdness: makeThirdness('organizational-law', 'structural-habit'),
  domain: 'spatiality',
  perspectiveIntegration: {
    firstPerson: '1st-person-space',
    secondPerson: '2nd-person-organization',
    thirdPerson: '3rd-person-architecture',
  },
  principle: 'Spatial organization through form',
};

export const exchangePattern: FirstOrderPattern = {
  type: 'first-order-pattern',
  name: 'exchange',
  order: 1,
  description: 'Flows, trades, and reciprocal exchanges between elements',
  firstness: makeFirstness('flow-quality', 'exchange-feeling'),
  secondness: makeSecondness('trading-existence', 'actual-exchange'),
  thirdness: makeThirdness('reciprocal-law', 'exchange-habit'),
  domain: 'relationality',
  perspectiveIntegration: {
    firstPerson: '1st-person-giving',
    secondPerson: '2nd-person-trading',
    thirdPerson: '3rd-person-economy',
  },
  principle: 'Relational coordination through reciprocity',
};

export const creativityPattern: FirstOrderPattern = {
  type: 'first-order-pattern',
  name: 'creativity',
  order: 1,
  description: 'Emergence of novelty, innovation, and creative transformation',
  firstness: makeFirstness('creative-quality', 'novel-feeling'),
  secondness: makeSecondness('emergent-existence', 'actual-novelty'),
  thirdness: makeThirdness('innovative-law', 'creative-habit'),
  domain: 'generativity',
  perspectiveIntegration: {
    firstPerson: '1st-person-inspiration',
    secondPerson: '2nd-person-innovation',
    thirdPerson: '3rd-person-evolution',
  },
  principle: 'Developmental transformation through novelty',
};

export const dynamicsPattern: FirstOrderPattern = {
  type: 'first-order-pattern',
  name: 'dynamics',
  order: 1,
  description: 'Dynamic processes, feedback loops, and systemic integration',
  firstness: makeFirstness('dynamic-quality', 'process-feeling'),
  secondness: makeSecondness('changing-existence', 'actual-process'),
  thirdness: makeThirdness('systemic-law', 'dynamic-habit'),
  domain: 'processuality',
  perspectiveIntegration: {
    firstPerson: '1st-person-change',
    secondPerson: '2nd-person-feedback',
    thirdPerson: '3rd-person-system',
  },
  principle: 'Process integration through feedback',
};

export const firstOrderPatterns: FirstOrderPattern[] = [
  rhythmPattern,
  polarityPattern,
  structurePattern,
  exchangePattern,
  creativityPattern,
  dynamicsPattern,
];

// ============================================================================
// Second-Order Patterns (42 patterns)
// ============================================================================

// Source Family (6 patterns)
const sourceDynamicsPattern = makeSecondOrderPattern(
  'source', 'dynamics',
  'energy', 'ground-transformations',
  'The fundamental animating force of reality - rhythmic vibration in space/time',
  'Drive dynamic change and transformation processes',
  'Dynamic transformation: balance energy use with availability'
);

const sourceCreativityPattern = makeSecondOrderPattern(
  'source', 'creativity',
  'pattern', 'dynamic-order',
  'Repeated types of order - recurrent design arrangements',
  'Provide successful templates of systemic design',
  'Potential and restrictiveness of form: balance patterned forms with unrestricted states'
);

const sourceExchangePattern = makeSecondOrderPattern(
  'source', 'exchange',
  'power', 'work-rate',
  'Productivity per unit time - rate and efficiency of systemic functioning',
  'Provide systemic productivity',
  'Productivity rates: balance work rate with resource flow'
);

const sourceStructurePattern = makeSecondOrderPattern(
  'source', 'structure',
  'transformity', 'order-accrual',
  'Qualitative complexification - transformation to higher complexity',
  'Bring advantages of structural increases in order',
  'Complexity creation: balance transformity with available energy'
);

const sourcePolarityPattern = makeSecondOrderPattern(
  'source', 'polarity',
  'resource', 'source-distinctions',
  'Process of transformation of matter/energy - conservation through transformation',
  'Provide different qualities of matter/energy for unique purposes',
  'Source transformations: balance number of transformations with usefulness'
);

const sourceRhythmPattern = makeSecondOrderPattern(
  'source', 'rhythm',
  'autopoiesis', 'self-creation',
  'Ongoing self-authoring - maintenance and creative unfolding',
  'Maintain unique system of order through creative growth',
  'Creative existence: balance self-development with self-maintenance'
);

export const sourceFamily: SecondOrderPattern[] = [
  sourceDynamicsPattern,
  sourceCreativityPattern,
  sourceExchangePattern,
  sourceStructurePattern,
  sourcePolarityPattern,
  sourceRhythmPattern,
];

// Dynamics Family (6 patterns)
const dynamicsDynamicsPattern = makeSecondOrderPattern(
  'dynamics', 'dynamics',
  'system', 'integrated-dynamics',
  'Ordered activity of a synergistic whole - integrated group of parts',
  'Maintain complex dynamic order',
  'Dynamic order: balance complex dynamic order with randomness'
);

const dynamicsCreativityPattern = makeSecondOrderPattern(
  'dynamics', 'creativity',
  'spontaneity', 'dynamic-response',
  'Coordinated impromptu reaction - instantaneous improvisation',
  'Provide seamless dynamic adaptation',
  'Impromptu adaptations: balance spontaneous reactions with perturbations'
);

const dynamicsExchangePattern = makeSecondOrderPattern(
  'dynamics', 'exchange',
  'feedback', 'adjustment-dynamics',
  'Dynamic adjustments through causal loops',
  'Make dynamic adjustments',
  'Causal loops: balance amount of feedback with size of adjustments'
);

const dynamicsStructurePattern = makeSecondOrderPattern(
  'dynamics', 'structure',
  'synergy', 'dynamical-structures',
  'Benefit through combination - whole greater than parts',
  'Provide a functional multiplier',
  'Structural integration: balance synergies with diversity of function'
);

const dynamicsPolarityPattern = makeSecondOrderPattern(
  'dynamics', 'polarity',
  'agency-communion', 'part-whole-polarity',
  'Tension between integration and disintegration',
  'Create and dissolve systems',
  'Formative boundaries: balance boundary formation with dissolution'
);

const dynamicsRhythmPattern = makeSecondOrderPattern(
  'dynamics', 'rhythm',
  'iterate', 'dynamic-rhythm',
  'Repeated cycles - incremental change through recurrent cycling',
  'Support ongoing incremental adaptations',
  'Cyclic adaptations: balance iterative speed with rate of change'
);

export const dynamicsFamily: SecondOrderPattern[] = [
  dynamicsDynamicsPattern,
  dynamicsCreativityPattern,
  dynamicsExchangePattern,
  dynamicsStructurePattern,
  dynamicsPolarityPattern,
  dynamicsRhythmPattern,
];

// Creativity Family (6 patterns)
const creativityDynamicsPattern = makeSecondOrderPattern(
  'creativity', 'dynamics',
  'evolution', 'dynamic-creativity',
  'Leap to a higher level of complexity - qualitative transformation',
  'Make developmental leaps',
  'Creative leaps: balance shifts to higher levels with foundational adaptations'
);

const creativityCreativityPattern = makeSecondOrderPattern(
  'creativity', 'creativity',
  'emergence', 'innovative-arising',
  'Moment of creative development - appearance of novel forms',
  'Launch new patterns of organization',
  'Innovative arising: balance emergence of novelty with energy for fusion'
);

const creativityExchangePattern = makeSecondOrderPattern(
  'creativity', 'exchange',
  'growth', 'creative-prosperity',
  'Developmental increase - compounding additions through cycles',
  'Create resources for system building',
  'Compound prosperity: balance exponential increase with resource availability'
);

const creativityStructurePattern = makeSecondOrderPattern(
  'creativity', 'structure',
  'adaptation', 'structural-adjustments',
  'Structural alterations within existing frameworks',
  'Adjust existing structures',
  'Structural modifications: balance modifications with functional integrity'
);

const creativityPolarityPattern = makeSecondOrderPattern(
  'creativity', 'polarity',
  'bifurcation', 'liminal-creation',
  'Point at which state change takes place - breakdown to new order',
  'Generate new forms of order from breakdown',
  'Creative chaos: balance depth of chaos with value of new order'
);

const creativityRhythmPattern = makeSecondOrderPattern(
  'creativity', 'rhythm',
  'seed', 'emergent-creation',
  'Repeated emergence of creative beginnings',
  'Emerge new opportunities',
  'Viable germination: balance number of seeds with their viability'
);

export const creativityFamily: SecondOrderPattern[] = [
  creativityDynamicsPattern,
  creativityCreativityPattern,
  creativityExchangePattern,
  creativityStructurePattern,
  creativityPolarityPattern,
  creativityRhythmPattern,
];

// Exchange Family (6 patterns)
const exchangeDynamicsPattern = makeSecondOrderPattern(
  'exchange', 'dynamics',
  'process', 'operation-dynamics',
  'Linear stage-by-stage development through sequence of steps',
  'Create value through sequential development',
  'Developmental process: balance number of steps with value creation'
);

const exchangeCreativityPattern = makeSecondOrderPattern(
  'exchange', 'creativity',
  'uniqueness', 'exchange-creation',
  'Difference between elements - variations creating distinctness',
  'Provide differentiation in form and function',
  'Essential distinctions: balance differentiation with similarity'
);

const exchangeExchangePattern = makeSecondOrderPattern(
  'exchange', 'exchange',
  'trade', 'essence-of-exchange',
  'Simple reciprocation - reciprocal exchange of unique resources',
  'Improve productivity through specialization',
  'Reciprocity: balance exchange of different resources'
);

const exchangeStructurePattern = makeSecondOrderPattern(
  'exchange', 'structure',
  'capture', 'flow-container',
  'Structure required to obtain yield from flow outside system',
  'Acquire resources for the system',
  'Obtaining a yield: balance capture of flows with structural capacity'
);

const exchangePolarityPattern = makeSecondOrderPattern(
  'exchange', 'polarity',
  'balance', 'relational-duality',
  'Dynamic equilibrium of relational reciprocity',
  'Generate systemic energy through exchanges',
  'Equitable exchange: balance giving with receiving'
);

const exchangeRhythmPattern = makeSecondOrderPattern(
  'exchange', 'rhythm',
  'cycle', 'exchange-phases',
  'Circuit of phases in interchange processes',
  'Provide sequence of repeated phases in any process',
  'Cyclic exchanges: balance phases in iterative circuit'
);

export const exchangeFamily: SecondOrderPattern[] = [
  exchangeDynamicsPattern,
  exchangeCreativityPattern,
  exchangeExchangePattern,
  exchangeStructurePattern,
  exchangePolarityPattern,
  exchangeRhythmPattern,
];

// Structure Family (6 patterns)
const structureDynamicsPattern = makeSecondOrderPattern(
  'structure', 'dynamics',
  'holarchy', 'structural-dynamics',
  'Nested arrangement of systems within systems',
  'Maintain order in dynamic growth of complexity',
  'Nested levels: balance number of levels with systems at each level'
);

const structureCreativityPattern = makeSecondOrderPattern(
  'structure', 'creativity',
  'complexity', 'order-creation',
  'Number of elements and connections in a system',
  'Creatively configure high degrees of order',
  'Interrelationships: balance numerous unique elements with simplicity'
);

const structureExchangePattern = makeSecondOrderPattern(
  'structure', 'exchange',
  'network', 'relational-design',
  'Inter-connective architecture of relationships - nodes and pathways',
  'Provide organizational clarity through interconnected distinctions',
  'Nodes and pathways: balance strength of relationships with node integrity'
);

const structureStructurePattern = makeSecondOrderPattern(
  'structure', 'structure',
  'hierarchy', 'essential-structure',
  'Essential property of system design - ranking of levels',
  'Generate gain through concentrated control supporting base',
  'Control as service: balance concentrated control with diffuse influence'
);

const structurePolarityPattern = makeSecondOrderPattern(
  'structure', 'polarity',
  'holon', 'part-whole',
  'Fundamental structural duality between parts and wholes',
  'Illustrate defining structural relationship of systems',
  'Part and whole: balance system\'s role as part with identity as whole'
);

const structureRhythmPattern = makeSecondOrderPattern(
  'structure', 'rhythm',
  'boundary', 'edge-pattern',
  'Design of the limiting edge of a system',
  'Maintain distinctions',
  'Edge design: balance structural definition with permeability'
);

export const structureFamily: SecondOrderPattern[] = [
  structureDynamicsPattern,
  structureCreativityPattern,
  structureExchangePattern,
  structureStructurePattern,
  structurePolarityPattern,
  structureRhythmPattern,
];

// Polarity Family (6 patterns)
const polarityDynamicsPattern = makeSecondOrderPattern(
  'polarity', 'dynamics',
  'competition-cooperation', 'dynamic-polarity',
  'Fundamental duality at systems level - striving vs collaboration',
  'Enhance functionality at systems level',
  'Competitive cooperation: balance competitive striving with cooperative synergies'
);

const polarityCreativityPattern = makeSecondOrderPattern(
  'polarity', 'creativity',
  'order-chaos', 'creative-polarity',
  'Oppositional dynamics in creative process',
  'Facilitate adaptation and evolution',
  'Creative breakdown: balance structured function with breakdown'
);

const polarityExchangePattern = makeSecondOrderPattern(
  'polarity', 'exchange',
  'flows-stores', 'exchange-states',
  'Dualistic form of exchangeable resources',
  'Ensure uninterrupted capacity for exchanges',
  'Augmented flows: balance resources in flow with resources in stocks'
);

const polarityStructurePattern = makeSecondOrderPattern(
  'polarity', 'structure',
  'input-output', 'polarity-structure',
  'Dualism in systemic structuring - resources needed vs wastes emitted',
  'Process resources',
  'Waste resources: balance input of resources with output of wastes'
);

const polarityPolarityPattern = makeSecondOrderPattern(
  'polarity', 'polarity',
  'concentration-diffusion', 'primordial-duality',
  'Foundational duality within systems - concentrated centers vs diffuse areas',
  'Leverage advantages of intensive centers',
  'Complex interconnections: balance flow to centers with distribution to hinterlands'
);

const polarityRhythmPattern = makeSecondOrderPattern(
  'polarity', 'rhythm',
  'expand-contract', 'rhythmic-duality',
  'Fundamental duality in any rhythmic movement or event',
  'Liberate energy through rhythmic interplay',
  'Rhythmic interplay: balance expansive movement with contractive movement'
);

export const polarityFamily: SecondOrderPattern[] = [
  polarityDynamicsPattern,
  polarityCreativityPattern,
  polarityExchangePattern,
  polarityStructurePattern,
  polarityPolarityPattern,
  polarityRhythmPattern,
];

// Rhythm Family (6 patterns)
const rhythmDynamicsPattern = makeSecondOrderPattern(
  'rhythm', 'dynamics',
  'enantiodromia', 'emergent-oppositions',
  'Force exerted by extreme movements on emergence of opposites',
  'Birth emergence of corrective dynamics',
  'Emergent opposites: balance energy of corrective dynamics to be generative'
);

const rhythmCreativityPattern = makeSecondOrderPattern(
  'rhythm', 'creativity',
  'synchronization', 'mesh-works',
  'Creative inter-meshing of elements and processes in time',
  'Find optimal arrangements of interconnections',
  'Creative interconnections: balance creative arrangements with stable functioning'
);

const rhythmExchangePattern = makeSecondOrderPattern(
  'rhythm', 'exchange',
  'pulse', 'flow-surges',
  'Repeated rhythmic surges of activity in resource flows',
  'Maximize exchange flows sustainably',
  'Peaks: balance rate of increase with rate of decline'
);

const rhythmStructurePattern = makeSecondOrderPattern(
  'rhythm', 'structure',
  'cadence', 'rhythm-structures',
  'Structuring of rhythms within systems - complex temporal coordination',
  'Provide complex rhythmic programming',
  'Complex timing: balance rhythmic complexity with simplicity'
);

const rhythmPolarityPattern = makeSecondOrderPattern(
  'rhythm', 'polarity',
  'swing', 'iterative-extremes',
  'Repeated movement toward one pole then back to opposite',
  'Maintain dynamic stability',
  'Dynamic stability: balance size of swings with frequency of adjustments'
);

const rhythmRhythmPattern = makeSecondOrderPattern(
  'rhythm', 'rhythm',
  'repetition', 'recurrent-order',
  'Simple ongoing recurrences in time and space',
  'Provide reliability',
  'Simple recurrence: balance reliability of repetition with need for variation'
);

export const rhythmFamily: SecondOrderPattern[] = [
  rhythmDynamicsPattern,
  rhythmCreativityPattern,
  rhythmExchangePattern,
  rhythmStructurePattern,
  rhythmPolarityPattern,
  rhythmRhythmPattern,
];

// ============================================================================
// Complete Pattern Holarchy
// ============================================================================

export const patternHolarchy: PatternHolarchy = {
  type: 'pattern-holarchy',
  totalPatterns: 49,
  structure: {
    zerothOrder: 1,
    firstOrder: 6,
    secondOrder: 42,
  },
  zerothOrderPatterns: [sourcePattern],
  firstOrderPatterns,
  secondOrderPatterns: {
    sourceFamily,
    dynamicsFamily,
    creativityFamily,
    exchangeFamily,
    structureFamily,
    polarityFamily,
    rhythmFamily,
  },
  nonDualOrigin,
  framework: 'integral-semiotic-realism',
  integration: 'pattern-dynamics',
};

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Get all 49 patterns as a flat array
 */
export function getAllPatterns(): (SourcePattern | FirstOrderPattern | SecondOrderPattern)[] {
  return [
    ...patternHolarchy.zerothOrderPatterns,
    ...patternHolarchy.firstOrderPatterns,
    ...patternHolarchy.secondOrderPatterns.sourceFamily,
    ...patternHolarchy.secondOrderPatterns.dynamicsFamily,
    ...patternHolarchy.secondOrderPatterns.creativityFamily,
    ...patternHolarchy.secondOrderPatterns.exchangeFamily,
    ...patternHolarchy.secondOrderPatterns.structureFamily,
    ...patternHolarchy.secondOrderPatterns.polarityFamily,
    ...patternHolarchy.secondOrderPatterns.rhythmFamily,
  ];
}

/**
 * Get patterns by order
 */
export function getPatternsByOrder(order: 0 | 1 | 2) {
  if (order === 0) return patternHolarchy.zerothOrderPatterns;
  if (order === 1) return patternHolarchy.firstOrderPatterns;
  
  return [
    ...patternHolarchy.secondOrderPatterns.sourceFamily,
    ...patternHolarchy.secondOrderPatterns.dynamicsFamily,
    ...patternHolarchy.secondOrderPatterns.creativityFamily,
    ...patternHolarchy.secondOrderPatterns.exchangeFamily,
    ...patternHolarchy.secondOrderPatterns.structureFamily,
    ...patternHolarchy.secondOrderPatterns.polarityFamily,
    ...patternHolarchy.secondOrderPatterns.rhythmFamily,
  ];
}

/**
 * Get patterns by major aspect
 */
export function getPatternsByMajorAspect(aspect: MajorAspect): SecondOrderPattern[] {
  const familyKey = `${aspect}Family` as keyof typeof patternHolarchy.secondOrderPatterns;
  return patternHolarchy.secondOrderPatterns[familyKey] || [];
}

/**
 * Find a pattern by name
 */
export function findPatternByName(name: string) {
  return getAllPatterns().find(p => p.name === name);
}

/**
 * Generate pattern graph for visualization
 */
export function generatePatternGraph(): PatternGraph {
  const nodes: PatternNode[] = [];
  const edges: PatternEdge[] = [];

  // Add source node
  nodes.push({
    id: 'source',
    name: 'Source',
    order: 0,
    type: 'source',
    description: sourcePattern.description,
  });

  // Add first-order nodes and edges from source
  firstOrderPatterns.forEach(pattern => {
    nodes.push({
      id: pattern.name,
      name: pattern.name.charAt(0).toUpperCase() + pattern.name.slice(1),
      order: 1,
      type: 'first-order',
      description: pattern.description,
    });
    
    edges.push({
      source: 'source',
      target: pattern.name,
      relationship: 'generates',
    });
  });

  // Add second-order nodes and edges
  const allSecondOrder = getPatternsByOrder(2);
  allSecondOrder.forEach(pattern => {
    nodes.push({
      id: pattern.name,
      name: pattern.name,
      order: 2,
      type: 'second-order',
      description: pattern.description,
      family: `${pattern.majorAspect}-family`,
    });

    edges.push({
      source: pattern.majorAspect,
      target: pattern.name,
      relationship: 'crosses-with',
      label: `${pattern.majorAspect} × ${pattern.minorAspect}`,
    });
  });

  return { nodes, edges };
}

/**
 * Get pattern count summary
 */
export function getPatternCountSummary() {
  return {
    total: 49,
    zerothOrder: 1,
    firstOrder: 6,
    secondOrder: 42,
    families: {
      source: 6,
      dynamics: 6,
      creativity: 6,
      exchange: 6,
      structure: 6,
      polarity: 6,
      rhythm: 6,
    },
  };
}

// Export all patterns and utilities
export {
  sourcePattern,
  firstOrderPatterns,
  sourceFamily,
  dynamicsFamily,
  creativityFamily,
  exchangeFamily,
  structureFamily,
  polarityFamily,
  rhythmFamily,
};
