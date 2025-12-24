/**
 * pattern-relational.data.ts
 * Complete relational matrix data from PD49Table
 * The 49 pattern relationships in the 7x7 matrix
 */

import type {
  CorePattern,
  PatternMatrix,
  GlobalProcess,
  HolarchicalRelation,
  createPatternRelationship,
  createGlobalProcess,
  createHolarchicalRelation
} from './pattern-relational.types';

// ============================================================================
// I. Complete 7x7 Pattern Relationship Matrix
// ============================================================================

export const PATTERN_MATRIX: PatternMatrix = {
  source: {
    source: {
      from: 'source',
      to: 'source',
      relationship: 'Ground Source',
      description: 'Unity Existence Perception - The foundational ground of being',
      cellNumber: undefined
    },
    dynamics: {
      from: 'source',
      to: 'dynamics',
      relationship: 'Unity Dynamics',
      description: 'Creative Grace - Source expressing as dynamic flow',
      cellNumber: 40
    },
    creative: {
      from: 'source',
      to: 'creative',
      relationship: 'Creative Grace',
      description: 'Source Stream - Creative arising from source',
      cellNumber: undefined
    },
    exchange: {
      from: 'source',
      to: 'exchange',
      relationship: 'Source Stream',
      description: 'Resonance Structure - Source flowing into exchange',
      cellNumber: undefined
    },
    structure: {
      from: 'source',
      to: 'structure',
      relationship: 'Resonance Structure',
      description: 'High Polarity - Structural resonance with source',
      cellNumber: undefined
    },
    polarity: {
      from: 'source',
      to: 'polarity',
      relationship: 'High Polarity',
      description: 'Field Vibrations - Polar tensions from source',
      cellNumber: undefined
    },
    rhythm: {
      from: 'source',
      to: 'rhythm',
      relationship: 'Field Vibrations',
      description: 'Unity Existence - Rhythmic field emanating from source',
      cellNumber: undefined
    }
  },
  dynamics: {
    source: {
      from: 'dynamics',
      to: 'source',
      relationship: 'Dynamic Order',
      description: 'Integrated Dynamics - Order arising from dynamic process',
      cellNumber: undefined
    },
    dynamics: {
      from: 'dynamics',
      to: 'dynamics',
      relationship: 'Integrated Dynamics',
      description: 'Dynamic Creativity - Self-referential dynamic process',
      cellNumber: 42
    },
    creative: {
      from: 'dynamics',
      to: 'creative',
      relationship: 'Dynamic Creativity',
      description: 'Operation Dynamics - Creative operations emerging from dynamics',
      cellNumber: undefined
    },
    exchange: {
      from: 'dynamics',
      to: 'exchange',
      relationship: 'Operation Dynamics',
      description: 'Structural Dynamics - Dynamic exchange processes',
      cellNumber: undefined
    },
    structure: {
      from: 'dynamics',
      to: 'structure',
      relationship: 'Structural Dynamics',
      description: 'Dynamic Polarity - Structure shaped by dynamics',
      cellNumber: undefined
    },
    polarity: {
      from: 'dynamics',
      to: 'polarity',
      relationship: 'Dynamic Polarity',
      description: 'Emergent Oppositions - Polar opposites through dynamics',
      cellNumber: undefined
    },
    rhythm: {
      from: 'dynamics',
      to: 'rhythm',
      relationship: 'Emergent Oppositions',
      description: 'Dynamics - Rhythmic dynamics emerging',
      cellNumber: undefined
    }
  },
  creative: {
    source: {
      from: 'creative',
      to: 'source',
      relationship: 'Self Creation',
      description: 'Dynamic Response - Creative self-generation',
      cellNumber: 41
    },
    dynamics: {
      from: 'creative',
      to: 'dynamics',
      relationship: 'Dynamic Response',
      description: 'Innovative Arising - Dynamic creative response',
      cellNumber: undefined
    },
    creative: {
      from: 'creative',
      to: 'creative',
      relationship: 'Innovative Arising',
      description: 'Exchange Creation - Creative innovation',
      cellNumber: undefined
    },
    exchange: {
      from: 'creative',
      to: 'exchange',
      relationship: 'Exchange Creation',
      description: 'Order Creation - Creative exchange processes',
      cellNumber: undefined
    },
    structure: {
      from: 'creative',
      to: 'structure',
      relationship: 'Order Creation',
      description: 'Creative Polarity - Structured creative forms',
      cellNumber: undefined
    },
    polarity: {
      from: 'creative',
      to: 'polarity',
      relationship: 'Creative Polarity',
      description: 'Mesh-Works - Creative polarities forming networks',
      cellNumber: undefined
    },
    rhythm: {
      from: 'creative',
      to: 'rhythm',
      relationship: 'Mesh-Works',
      description: 'Disjunction/Intersection - Creative rhythmic patterns',
      cellNumber: undefined
    }
  },
  exchange: {
    source: {
      from: 'exchange',
      to: 'source',
      relationship: 'Work Rate',
      description: 'Adjustment Dynamics - Exchange work processes',
      cellNumber: 39
    },
    dynamics: {
      from: 'exchange',
      to: 'dynamics',
      relationship: 'Adjustment Dynamics',
      description: 'Creative Prosperity - Dynamic exchange adjustments',
      cellNumber: undefined
    },
    creative: {
      from: 'exchange',
      to: 'creative',
      relationship: 'Creative Prosperity',
      description: 'Essence of Exchange - Creative exchange',
      cellNumber: undefined
    },
    exchange: {
      from: 'exchange',
      to: 'exchange',
      relationship: 'Essence of Exchange',
      description: 'Relational Design - Core exchange processes',
      cellNumber: undefined
    },
    structure: {
      from: 'exchange',
      to: 'structure',
      relationship: 'Relational Design',
      description: 'Exchange States - Structured exchange relationships',
      cellNumber: undefined
    },
    polarity: {
      from: 'exchange',
      to: 'polarity',
      relationship: 'Exchange States',
      description: 'Flow Surges - Polar exchange flows',
      cellNumber: 29
    },
    rhythm: {
      from: 'exchange',
      to: 'rhythm',
      relationship: 'Flow Surges',
      description: 'Exchange - Rhythmic exchange patterns',
      cellNumber: undefined
    }
  },
  structure: {
    source: {
      from: 'structure',
      to: 'source',
      relationship: 'Order Accrual',
      description: 'Dynamical Structures - Accumulation of structural order',
      cellNumber: undefined
    },
    dynamics: {
      from: 'structure',
      to: 'dynamics',
      relationship: 'Dynamical Structures',
      description: 'Structural Adjustments - Dynamic structural patterns',
      cellNumber: undefined
    },
    creative: {
      from: 'structure',
      to: 'creative',
      relationship: 'Structural Adjustments',
      description: 'Flow Container - Creative structural forms',
      cellNumber: undefined
    },
    exchange: {
      from: 'structure',
      to: 'exchange',
      relationship: 'Flow Container',
      description: 'Essential Structure - Structural exchange channels',
      cellNumber: undefined
    },
    structure: {
      from: 'structure',
      to: 'structure',
      relationship: 'Essential Structure',
      description: 'Polarity Structure - Core structural patterns',
      cellNumber: undefined
    },
    polarity: {
      from: 'structure',
      to: 'polarity',
      relationship: 'Polarity Structure',
      description: 'Rhythm Structures - Polar structural tensions',
      cellNumber: undefined
    },
    rhythm: {
      from: 'structure',
      to: 'rhythm',
      relationship: 'Rhythm Structures',
      description: 'Structure - Rhythmic structural patterns',
      cellNumber: undefined
    }
  },
  polarity: {
    source: {
      from: 'polarity',
      to: 'source',
      relationship: 'Source Distinctions',
      description: 'Part / Whole Polarity - Fundamental distinctions',
      cellNumber: 23
    },
    dynamics: {
      from: 'polarity',
      to: 'dynamics',
      relationship: 'Part / Whole Polarity',
      description: 'Liminal Creation - Dynamic polar distinctions',
      cellNumber: undefined
    },
    creative: {
      from: 'polarity',
      to: 'creative',
      relationship: 'Liminal Creation',
      description: 'Relational Duality - Creative polarity',
      cellNumber: undefined
    },
    exchange: {
      from: 'polarity',
      to: 'exchange',
      relationship: 'Relational Duality',
      description: 'Part / Whole - Polar exchange relationships',
      cellNumber: undefined
    },
    structure: {
      from: 'polarity',
      to: 'structure',
      relationship: 'Part / Whole',
      description: 'Primordial Duality - Structural polarities',
      cellNumber: undefined
    },
    polarity: {
      from: 'polarity',
      to: 'polarity',
      relationship: 'Primordial Duality',
      description: 'Iterative Extremes - Self-referential polarity',
      cellNumber: undefined
    },
    rhythm: {
      from: 'polarity',
      to: 'rhythm',
      relationship: 'Iterative Extremes',
      description: 'Complementarity Distinction - Rhythmic polar patterns',
      cellNumber: undefined
    }
  },
  rhythm: {
    source: {
      from: 'rhythm',
      to: 'source',
      relationship: 'Ground Transformations',
      description: 'Dynamic Rhythm - Transformative rhythms from source',
      cellNumber: 17
    },
    dynamics: {
      from: 'rhythm',
      to: 'dynamics',
      relationship: 'Dynamic Rhythm',
      description: 'Emergent Creation - Rhythmic dynamics',
      cellNumber: undefined
    },
    creative: {
      from: 'rhythm',
      to: 'creative',
      relationship: 'Emergent Creation',
      description: 'Exchange Phases - Creative rhythmic phases',
      cellNumber: undefined
    },
    exchange: {
      from: 'rhythm',
      to: 'exchange',
      relationship: 'Exchange Phases',
      description: 'Edge Pattern - Rhythmic exchange boundaries',
      cellNumber: 28
    },
    structure: {
      from: 'rhythm',
      to: 'structure',
      relationship: 'Edge Pattern',
      description: 'Rhythmic Duality - Structural rhythms',
      cellNumber: undefined
    },
    polarity: {
      from: 'rhythm',
      to: 'polarity',
      relationship: 'Rhythmic Duality',
      description: 'Recurrent Order - Polar rhythmic patterns',
      cellNumber: undefined
    },
    rhythm: {
      from: 'rhythm',
      to: 'rhythm',
      relationship: 'Recurrent Order',
      description: 'Rhythm - Self-referential rhythmic cycles',
      cellNumber: undefined
    }
  }
} as const;

// ============================================================================
// II. Global Processes (numbered references from PD49Table)
// ============================================================================

export const GLOBAL_PROCESSES: readonly GlobalProcess[] = [
  {
    type: 'distinction',
    scope: 'global',
    number: 17,
    mechanism: 'expansion/contraction, input/output',
    examples: ['Rhythm-Source boundary formation', 'Modular recursion']
  },
  {
    type: 'unfolding',
    scope: 'global',
    number: 23,
    mechanism: 'bifurcation, conflict, fission',
    examples: ['Polarity-Source differentiation', 'Part/Whole emergence']
  },
  {
    type: 'folding',
    scope: 'global',
    number: 28,
    mechanism: 'boundary formation',
    examples: ['Rhythm-Exchange integration', 'Phase coordination']
  },
  {
    type: 'folding',
    scope: 'global',
    number: 29,
    mechanism: 'pulse, cooperation, fusion',
    examples: ['Exchange-Polarity convergence', 'Flow surge coordination']
  },
  {
    type: 'closure',
    scope: 'local',
    number: 39,
    mechanism: 'feedback, homeostasis, self-control, auto-dynamic',
    examples: ['Exchange-Source regulation', 'Work rate adjustment']
  },
  {
    type: 'closure',
    scope: 'global',
    number: 40,
    mechanism: 'Unity dynamics',
    examples: ['Source-Dynamics integration', 'Creative grace']
  },
  {
    type: 'closure',
    scope: 'global',
    number: 41,
    mechanism: 'limit cycle, eigen-behaviour, autopoiesis',
    examples: ['Creative-Source self-generation', 'Dynamic response']
  },
  {
    type: 'closure',
    scope: 'global',
    number: 42,
    mechanism: 'Integrated dynamics',
    examples: ['Dynamics-Dynamics self-reference', 'Operational creativity']
  }
] as const;

// ============================================================================
// III. Holarchical Relationships
// ============================================================================

export const HOLARCHICAL_RELATIONS: readonly HolarchicalRelation[] = [
  // Source as foundation
  {
    type: 'emerges-from',
    parent: 'dynamics',
    child: 'source',
    level: 0,
    description: 'Dynamics emerges from Source as creative grace'
  },
  {
    type: 'emerges-from',
    parent: 'creative',
    child: 'source',
    level: 0,
    description: 'Creative emerges from Source through self-creation'
  },
  
  // Dynamics level
  {
    type: 'coordinates',
    parent: 'dynamics',
    child: 'rhythm',
    level: 1,
    description: 'Dynamics coordinates rhythmic patterns'
  },
  
  // Creative-Polarity pairing
  {
    type: 'transcends',
    parent: 'creative',
    child: 'polarity',
    level: 2,
    description: 'Creative transcends polarity through innovative arising',
    bidirectional: true
  },
  
  // Structure-Exchange pairing
  {
    type: 'contains',
    parent: 'structure',
    child: 'exchange',
    level: 3,
    description: 'Structure contains and shapes exchange flows'
  },
  {
    type: 'regulates',
    parent: 'exchange',
    child: 'structure',
    level: 3,
    description: 'Exchange regulates structural formation',
    bidirectional: true
  },
  
  // Cross-level relationships
  {
    type: 'composes',
    parent: 'rhythm',
    child: 'source',
    level: 1,
    description: 'Rhythm composes back to Source through ground transformations'
  },
  {
    type: 'includes',
    parent: 'polarity',
    child: 'dynamics',
    level: 2,
    description: 'Polarity includes dynamic oppositions'
  },
  {
    type: 'includes',
    parent: 'structure',
    child: 'dynamics',
    level: 3,
    description: 'Structure includes dynamical structures'
  }
] as const;

// ============================================================================
// IV. Meta-Patterns (Second Order Patterns)
// ============================================================================

export interface MetaPattern {
  readonly name: string;
  readonly composedFrom: readonly CorePattern[];
  readonly description: string;
  readonly emergentProperty: string;
}

export const META_PATTERNS: readonly MetaPattern[] = [
  {
    name: 'Unity Field',
    composedFrom: ['source', 'rhythm'],
    description: 'The continuous field of existence and perception',
    emergentProperty: 'Field Vibrations - Unity consciousness'
  },
  {
    name: 'Dynamic Flow',
    composedFrom: ['dynamics', 'creative'],
    description: 'The creative flow of change and innovation',
    emergentProperty: 'Innovative Arising - Dynamic creativity'
  },
  {
    name: 'Relational Network',
    composedFrom: ['exchange', 'structure'],
    description: 'The organized network of relationships and flows',
    emergentProperty: 'Relational Design - Structured exchange'
  },
  {
    name: 'Polar Tension',
    composedFrom: ['polarity', 'creative'],
    description: 'The creative tension between opposites',
    emergentProperty: 'Mesh-Works - Creative polarities'
  },
  {
    name: 'Autopoietic Loop',
    composedFrom: ['dynamics', 'exchange', 'structure'],
    description: 'Self-maintaining organizational closure',
    emergentProperty: 'Integrated self-production'
  },
  {
    name: 'Transcendent Holarchy',
    composedFrom: ['source', 'dynamics', 'creative', 'exchange', 'structure', 'polarity', 'rhythm'],
    description: 'The complete integrated pattern system',
    emergentProperty: 'Unified consciousness operating system'
  }
] as const;

// ============================================================================
// V. Relational Principles
// ============================================================================

export interface RelationalPrinciple {
  readonly name: string;
  readonly principle: string;
  readonly patterns: readonly CorePattern[];
}

export const RELATIONAL_PRINCIPLES: readonly RelationalPrinciple[] = [
  {
    name: 'Dynamic Stability',
    principle: 'The enduring health of any system depends on the appropriate balance and integration between the size of the swings to extremes and the frequency of adjustments around the middle path',
    patterns: ['dynamics', 'polarity', 'rhythm']
  },
  {
    name: 'Complex Timing',
    principle: 'Balance of rhythmic complexity with rhythmic simplicity',
    patterns: ['rhythm', 'structure']
  },
  {
    name: 'Peak Management',
    principle: 'Balance the rate of increase in resource flows and exchanges pre-peak with the rate of decline in those flows and exchanges after the peak',
    patterns: ['exchange', 'dynamics', 'rhythm']
  },
  {
    name: 'Field Harmonics',
    principle: 'The enduring health and evolution of any system depends on the appropriate balance and integration of the influence of the harmonic field with the capacity for independent expression',
    patterns: ['source', 'rhythm', 'polarity']
  },
  {
    name: 'Resonance Pattern',
    principle: 'Sympathetic coordination of elements within a system through fields of subtle interconnections',
    patterns: ['source', 'structure', 'rhythm']
  }
] as const;
