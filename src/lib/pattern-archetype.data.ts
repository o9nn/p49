/**
 * Pattern Archetype Data Provider
 * 
 * Provides structured data based on the Scheme Pattern Archetype model
 * for use in the TypeScript/React visualization components
 */

import {
  makeFirstness,
  makeSecondness,
  makeThirdness,
  makeDynamicPattern,
  NONDUAL_ORIGIN,
  PERSPECTIVE_MAP,
  type IntegralArchetype,
  type SemioticCategory,
  type DynamicPattern,
} from './pattern-archetype.types';

// ============================================================================
// Example Archetype Instance (matches the Scheme example)
// ============================================================================

/**
 * Create an example integral archetype for consciousness
 * This mirrors the example-archetype defined in the Scheme implementation
 */
export function createExampleArchetype(): IntegralArchetype {
  const firstness = makeFirstness(
    'phenomenal-experience',
    'immediate-quality'
  );
  
  const secondness = makeSecondness(
    'physical-reality',
    'brute-existence'
  );
  
  const thirdness = makeThirdness(
    'conceptual-understanding',
    'mediating-law'
  );

  const metaPattern = makeDynamicPattern(
    'consciousness-pattern',
    ['subject-object-relation'],
    'evolutionary-complexification'
  );

  return {
    type: 'integral-archetype',
    name: 'Integral Semiotic Realism Pattern',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [firstness, secondness, thirdness],
      processes: [
        {
          type: 'continuous-signification',
          process: 'method',
          from: 'firstness',
          through: 'thirdness',
          to: 'secondness',
          sign: 'phenomenal-experience',
          interpretant: 'conceptual-understanding',
          object: 'physical-reality',
          description: 'The ongoing process of sign interpretation and meaning-making',
        },
        {
          type: 'epistemic-emergence',
          from: 'nondual-origin',
          to: 'firstness',
          origin: NONDUAL_ORIGIN,
          emergent: 'phenomenal-experience',
          direction: 'subjective',
          description: 'The emergence of epistemic awareness from undifferentiated origin',
        },
        {
          type: 'ontological-emergence',
          from: 'nondual-origin',
          to: 'secondness',
          origin: NONDUAL_ORIGIN,
          emergent: 'physical-reality',
          direction: 'objective',
          description: 'The emergence of ontological reality from undifferentiated origin',
        },
      ],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'The dynamic cycle of emergence from and return to NonDual Origin',
      },
      domains: [
        {
          type: 'actual-domain',
          nature: 'manifest',
          contents: ['epistemic-signs', 'ontological-objects'],
          description: 'Domain of actualized forms where signs and objects exist',
        },
        {
          type: 'intransitive-domain',
          nature: 'relational',
          zone: 'subsistence',
          contents: ['semiotic-processes', 'meaning-making'],
          description: 'Realm of pure relationality before actualization',
        },
        {
          type: 'empirical-domain',
          nature: 'experiential',
          zone: 'existence',
          contents: ['enactment', 'practice'],
          description: 'Realm of lived experience and empirical manifestation',
        },
      ],
    },
    metaPattern,
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'The complete pattern integrating triadic semiotics with nondual origin',
  };
}

// ============================================================================
// Node Data for Visualization
// ============================================================================

/**
 * Convert a SemioticCategory to the Node format used by the React components
 */
export function semioticCategoryToNode(category: SemioticCategory, position: { x: number; y: number }) {
  const baseNode = {
    x: position.x,
    y: position.y,
  };

  switch (category.type) {
    case 'firstness':
      return {
        id: 'firstness',
        label: 'Firstness/Sign',
        color: 'var(--firstness)',
        perspective: '1st Person Perspective/Subject',
        domain: '[epistemology]',
        description: 'The immediate, qualitative aspect of experience. Pure possibility and potentiality before interpretation.',
        ...baseNode,
      };
    
    case 'secondness':
      return {
        id: 'secondness',
        label: 'Secondness/Object',
        color: 'var(--secondness)',
        perspective: '3rd Person Perspective/Object',
        domain: '[ontology]',
        description: 'The actual, brute fact of existence. Reality as resistance, reaction, and concrete particularity.',
        ...baseNode,
      };
    
    case 'thirdness':
      return {
        id: 'thirdness',
        label: 'Thirdness/Interpretant',
        color: 'var(--thirdness)',
        perspective: '2nd Person Perspective',
        domain: '[methodology]',
        description: 'The mediating principle of meaning, habit, and general law. The interpretation that connects sign and object.',
        ...baseNode,
      };
  }
}

/**
 * Get all nodes from the archetype for visualization
 */
export function getArchetypeNodes(archetype: IntegralArchetype) {
  const width = 800;
  const height = 600;
  const centerX = width / 2;
  const centerY = height / 2;

  const [firstness, secondness, thirdness] = archetype.semioticCycle.nodes;

  return [
    semioticCategoryToNode(firstness, { x: centerX - 200, y: centerY - 50 }),
    semioticCategoryToNode(secondness, { x: centerX + 200, y: centerY - 50 }),
    semioticCategoryToNode(thirdness, { x: centerX, y: centerY - 180 }),
  ];
}

// ============================================================================
// Path/Process Information for Visualization
// ============================================================================

export interface PathInfo {
  id: string;
  label: string;
  description: string;
  from: string;
  to: string;
  type: 'epistemic' | 'ontological' | 'signification' | 'return';
}

/**
 * Get all paths/processes from the archetype
 */
export function getArchetypePaths(archetype: IntegralArchetype): PathInfo[] {
  return [
    {
      id: 'epistemic-emergence',
      label: 'epistemic emergence',
      description: 'The emergence of epistemic awareness from undifferentiated origin',
      from: 'nondual-origin',
      to: 'firstness',
      type: 'epistemic',
    },
    {
      id: 'ontological-emergence',
      label: 'ontological emergence',
      description: 'The emergence of ontological reality from undifferentiated origin',
      from: 'nondual-origin',
      to: 'secondness',
      type: 'ontological',
    },
    {
      id: 'continuous-signification-upper',
      label: 'continuous signification (method)',
      description: 'The ongoing process of sign interpretation through the interpretant',
      from: 'firstness',
      to: 'secondness',
      type: 'signification',
    },
    {
      id: 'continuous-signification-lower',
      label: 'continuous signification (method)',
      description: 'The enactment process in the empirical domain',
      from: 'secondness',
      to: 'firstness',
      type: 'signification',
    },
    {
      id: 'nondual-return-left',
      label: 'nondual return',
      description: 'The dynamic return to undifferentiated origin',
      from: 'firstness',
      to: 'nondual-origin',
      type: 'return',
    },
    {
      id: 'nondual-return-right',
      label: 'nondual return',
      description: 'The dynamic return to undifferentiated origin',
      from: 'secondness',
      to: 'nondual-origin',
      type: 'return',
    },
  ];
}

// ============================================================================
// Domain Information
// ============================================================================

export interface DomainInfo {
  id: string;
  name: string;
  description: string;
  color: string;
  zone?: string;
}

/**
 * Get domain information from the archetype
 */
export function getArchetypeDomains(archetype: IntegralArchetype): DomainInfo[] {
  return [
    {
      id: 'actual-domain',
      name: 'Actual Domain',
      description: archetype.semioticCycle.domains[0].description,
      color: 'var(--firstness)',
    },
    {
      id: 'intransitive-domain',
      name: 'Intransitive Domain',
      description: archetype.semioticCycle.domains[1].description,
      color: 'var(--thirdness)',
      zone: 'Zone of Subsistence',
    },
    {
      id: 'empirical-domain',
      name: 'Empirical Domain',
      description: archetype.semioticCycle.domains[2].description,
      color: 'var(--secondness)',
      zone: 'Zone of Existence',
    },
  ];
}

// ============================================================================
// Perspective Information
// ============================================================================

export interface PerspectiveInfo {
  perspective: '1st-person' | '2nd-person' | '3rd-person';
  category: 'firstness' | 'secondness' | 'thirdness';
  aspect: 'subject' | 'interpretant' | 'object';
  domain: 'epistemology' | 'methodology' | 'ontology';
  description: string;
}

/**
 * Get perspective information from the archetype
 */
export function getArchetypePerspectives(archetype: IntegralArchetype): PerspectiveInfo[] {
  return [
    {
      perspective: '1st-person',
      category: 'firstness',
      aspect: 'subject',
      domain: 'epistemology',
      description: 'Subjective experience and immediate awareness',
    },
    {
      perspective: '2nd-person',
      category: 'thirdness',
      aspect: 'interpretant',
      domain: 'methodology',
      description: 'Intersubjective meaning-making and interpretation',
    },
    {
      perspective: '3rd-person',
      category: 'secondness',
      aspect: 'object',
      domain: 'ontology',
      description: 'Objective reality and brute existence',
    },
  ];
}

// ============================================================================
// Export the example archetype
// ============================================================================

export const EXAMPLE_ARCHETYPE = createExampleArchetype();
