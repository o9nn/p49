/**
 * Extended Use Cases for Integral Semiotic Realism
 * 
 * Demonstrates application of the framework across diverse domains
 * Based on examples from integral-semiotic-enactment.md
 */

import {
  makeFirstness,
  makeSecondness,
  makeThirdness,
  makeDynamicPattern,
  type IntegralArchetype,
  NONDUAL_ORIGIN,
  ACTUAL_DOMAIN,
  INTRANSITIVE_DOMAIN,
  EMPIRICAL_DOMAIN,
  PERSPECTIVE_MAP,
} from './pattern-archetype.types';

// ============================================================================
// Use Case 1: Ecosystem - Complex Adaptive Systems
// ============================================================================

export function createEcosystemArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Ecosystem Dynamics',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('species-interactions', 'phenomenal-patterns'),
        makeSecondness('ecological-processes', 'actual-mechanisms'),
        makeThirdness('ecosystem-function', 'systemic-organization'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Dynamic ecosystem evolution',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'ecosystem-dynamics',
      ['food-web', 'nutrient-cycling', 'succession'],
      'homeostasis-and-adaptation'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Ecosystem as an integral semiotic pattern demonstrating complex adaptive systems',
  };
}

// ============================================================================
// Use Case 2: Technology Evolution
// ============================================================================

export function createTechnologyEvolutionArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Technology Evolution',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('user-experience', 'interface-quality'),
        makeSecondness('technical-substrate', 'implementation'),
        makeThirdness('design-patterns', 'architectural-organization'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Technology paradigm evolution',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'tech-paradigm',
      ['interface', 'hardware-software', 'integration'],
      'increasing-abstraction'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Technology development as evolving paradigm shifts toward simplification',
  };
}

// ============================================================================
// Use Case 3: Social Movements
// ============================================================================

export function createSocialMovementArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Social Movement',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('collective-emotion', 'shared-passion'),
        makeSecondness('structural-conditions', 'material-reality'),
        makeThirdness('ideological-framing', 'meaning-narrative'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Social movement evolution',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'movement-dynamics',
      ['mobilization', 'action', 'institutionalization'],
      'progressive-change'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Social movements as collective action driving progressive change',
  };
}

// ============================================================================
// Use Case 4: Learning Process
// ============================================================================

export function createLearningProcessArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Learning Process',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('student-experience', 'subjective-understanding'),
        makeSecondness('objective-content', 'knowledge-to-learn'),
        makeThirdness('pedagogical-method', 'learning-methodology'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Educational development cycle',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'educational-development',
      ['scaffolding', 'practice', 'mastery'],
      'increasing-competence'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Learning as developmental journey from novice to mastery',
  };
}

// ============================================================================
// Use Case 5: Therapeutic Process
// ============================================================================

export function createTherapyProcessArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Therapeutic Process',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('client-experience', 'felt-sense'),
        makeSecondness('behavioral-patterns', 'actual-actions'),
        makeThirdness('therapeutic-relationship', 'meaning-making-space'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Healing transformation cycle',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'healing-journey',
      ['awareness', 'insight', 'integration'],
      'transformation'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Therapeutic process as transformative healing journey',
  };
}

// ============================================================================
// Use Case 6: Decision-Making
// ============================================================================

export function createDecisionMakingArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Decision-Making Process',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('intuitive-sense', 'gut-feeling'),
        makeSecondness('situational-reality', 'actual-constraints'),
        makeThirdness('deliberative-reasoning', 'analytical-process'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Decision cycle from intuition to action',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'decision-process',
      ['option-evaluation', 'commitment-formation'],
      'adaptive-optimization'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Decision-making integrating intuition, analysis, and enactment',
  };
}

// ============================================================================
// Use Case 7: Organizational Culture
// ============================================================================

export function createOrganizationalCultureArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Organizational Culture',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('shared-values', 'collective-identity'),
        makeSecondness('organizational-structure', 'hierarchy-processes'),
        makeThirdness('cultural-practices', 'how-things-are-done'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Cultural evolution cycle',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'culture-dynamics',
      ['norms', 'rituals', 'identity'],
      'cultural-evolution'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Organizational culture as evolving collective identity',
  };
}

// ============================================================================
// Classic Domain Examples
// ============================================================================

export function createPerceptionArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Perception Process',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('sensory-qualia', 'immediate-impression'),
        makeSecondness('physical-stimulus', 'external-object'),
        makeThirdness('perceptual-recognition', 'categorization'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Perceptual cycle',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'perception-cycle',
      ['sensation', 'attention', 'interpretation'],
      'perceptual-learning'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Perception as continuous semiotic process',
  };
}

export function createLanguageArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Language System',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('utterance', 'spoken-written-sign'),
        makeSecondness('referent', 'thing-referred-to'),
        makeThirdness('semantic-meaning', 'understanding'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Linguistic semiosis',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'linguistic-cycle',
      ['phonology', 'syntax', 'semantics'],
      'linguistic-evolution'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Language as triadic semiotic system',
  };
}

export function createScientificKnowledgeArchetype(): IntegralArchetype {
  return {
    type: 'integral-archetype',
    name: 'Scientific Knowledge',
    semioticCycle: {
      type: 'semiotic-cycle',
      nodes: [
        makeFirstness('observation', 'empirical-data'),
        makeSecondness('natural-phenomenon', 'actual-processes'),
        makeThirdness('theoretical-model', 'scientific-theory'),
      ],
      processes: [],
      origin: NONDUAL_ORIGIN,
      evolution: {
        type: 'nondual-evolution',
        leftCycle: ['epistemic-emergence', 'firstness', 'nondual-return'],
        rightCycle: ['ontological-emergence', 'secondness', 'nondual-return'],
        description: 'Scientific method cycle',
      },
      domains: [ACTUAL_DOMAIN, INTRANSITIVE_DOMAIN, EMPIRICAL_DOMAIN],
    },
    metaPattern: makeDynamicPattern(
      'scientific-method',
      ['hypothesis', 'experiment', 'theory'],
      'knowledge-accumulation'
    ),
    nonDualOrigin: NONDUAL_ORIGIN,
    perspectivalSystem: PERSPECTIVE_MAP,
    description: 'Scientific knowledge as evolving understanding',
  };
}

// ============================================================================
// Use Case Registry
// ============================================================================

export const USE_CASE_ARCHETYPES = {
  ecosystem: createEcosystemArchetype,
  technology: createTechnologyEvolutionArchetype,
  socialMovement: createSocialMovementArchetype,
  learning: createLearningProcessArchetype,
  therapy: createTherapyProcessArchetype,
  decisionMaking: createDecisionMakingArchetype,
  organizationalCulture: createOrganizationalCultureArchetype,
  perception: createPerceptionArchetype,
  language: createLanguageArchetype,
  scientificKnowledge: createScientificKnowledgeArchetype,
} as const;

export type UseCaseKey = keyof typeof USE_CASE_ARCHETYPES;

export function getAllUseCaseArchetypes(): Record<UseCaseKey, IntegralArchetype> {
  const result = {} as Record<UseCaseKey, IntegralArchetype>;
  
  for (const key in USE_CASE_ARCHETYPES) {
    const typedKey = key as UseCaseKey;
    result[typedKey] = USE_CASE_ARCHETYPES[typedKey]();
  }
  
  return result;
}

export function getUseCaseArchetype(key: UseCaseKey): IntegralArchetype {
  return USE_CASE_ARCHETYPES[key]();
}
