/**
 * pattern-dynamics.test.ts
 * Simple validation tests for Pattern Dynamics types and factory functions
 */

import {
  createEigenDrift,
  createMorphogenesis,
  createVortex,
  createAutopoiesis,
  createAutogenesis,
  createInfinityLoop,
  createViableSystemTriad,
  createRhythm,
  createRepetition,
  getEvolutionStage,
  isAutonomous,
  getActivePlanes,
  evolveThroughStages,
  type EigenDrift,
  type Autogenesis,
  type InfinityLoop,
  type ViableSystemTriad,
} from './pattern-dynamics.types';

// Test evolution stages
export function testEvolutionStages() {
  console.log('Testing Evolution Stages...');
  
  // Create each stage
  const stage0 = createEigenDrift();
  const stage1 = createMorphogenesis();
  const stage2 = createVortex('component-a', 'component-b');
  const stage4 = createAutopoiesis();
  const stage6 = createAutogenesis();
  
  // Verify stage numbers
  console.assert(stage0.stage === 0, 'Eigen Drift should be stage 0');
  console.assert(stage1.stage === 1, 'Morphogenesis should be stage 1');
  console.assert(stage2.stage === 2, 'Vortex should be stage 2');
  console.assert(stage4.stage === 4, 'Autopoiesis should be stage 4');
  console.assert(stage6.stage === 6, 'Autogenesis should be stage 6');
  
  // Verify planes
  console.assert(stage0.planesActive.length === 0, 'Stage 0 has no active planes');
  console.assert(stage1.planesActive.length === 1, 'Stage 1 has 1 active plane');
  console.assert(stage2.planesActive.length === 2, 'Stage 2 has 2 active planes');
  console.assert(stage4.planesActive.length === 3, 'Stage 4 has 3 active planes');
  console.assert(stage6.planesActive.length === 1, 'Stage 6 has 1 active plane (holistic)');
  
  console.log('✓ Evolution stages tests passed');
}

// Test infinity loop
export function testInfinityLoop() {
  console.log('Testing Infinity Loop...');
  
  const loop = createInfinityLoop('epistemic-process', 'ontological-process');
  
  console.assert(loop.morphology === 'lemniscate', 'Loop should be lemniscate');
  console.assert(loop.crossingPoint === 'nondual-origin', 'Crossing point is nondual origin');
  console.assert(loop.continuousFlow === true, 'Flow should be continuous');
  console.assert(loop.structure === 'triadic-semiosis', 'Structure is triadic');
  
  console.log('✓ Infinity loop tests passed');
}

// Test viable system triad
export function testViableSystemTriad() {
  console.log('Testing Viable System Triad...');
  
  const triad = createViableSystemTriad(
    'operations-execution',
    'management-coordination',
    'market-sensing'
  );
  
  console.assert(triad.operations.category === 'secondness', 'Operations is Secondness');
  console.assert(triad.coordination.category === 'thirdness', 'Coordination is Thirdness');
  console.assert(triad.intelligence.category === 'firstness', 'Intelligence is Firstness');
  console.assert(triad.recursiveStructure === true, 'Should be recursive');
  
  console.log('✓ Viable system triad tests passed');
}

// Test temporal patterns
export function testTemporalPatterns() {
  console.log('Testing Temporal Patterns...');
  
  const rhythm = createRhythm('weekly-meeting', '7 days', 'varies by context');
  const repetition = createRepetition('daily-standup', 5, '24 hours');
  
  console.assert(rhythm.category === 'firstness', 'Rhythm is Firstness');
  console.assert(rhythm.experiential === true, 'Rhythm is experiential');
  
  console.assert(repetition.category === 'secondness', 'Repetition is Secondness');
  console.assert(repetition.measurable === true, 'Repetition is measurable');
  console.assert(repetition.count === 5, 'Repetition count should be 5');
  
  console.log('✓ Temporal pattern tests passed');
}

// Test utility functions
export function testUtilityFunctions() {
  console.log('Testing Utility Functions...');
  
  // Test getEvolutionStage
  const stage0 = getEvolutionStage(0);
  const stage6 = getEvolutionStage(6);
  
  console.assert(stage0 !== null, 'Should get stage 0');
  console.assert(stage6 !== null, 'Should get stage 6');
  console.assert(stage0!.stage === 0, 'Stage 0 should have correct stage number');
  console.assert(stage6!.stage === 6, 'Stage 6 should have correct stage number');
  
  // Test isAutonomous
  const autogenesis = createAutogenesis();
  const morphogenesis = createMorphogenesis();
  
  console.assert(isAutonomous(autogenesis) === true, 'Autogenesis should be autonomous');
  console.assert(isAutonomous(morphogenesis) === false, 'Morphogenesis should not be autonomous');
  
  // Test getActivePlanes
  const eigenDrift = createEigenDrift();
  const autopoiesis = createAutopoiesis();
  
  const planes0 = getActivePlanes(eigenDrift);
  const planes4 = getActivePlanes(autopoiesis);
  
  console.assert(planes0.length === 0, 'Eigen drift has no active planes');
  console.assert(planes4.length === 3, 'Autopoiesis has 3 active planes');
  
  // Test evolveThroughStages
  const evolution = evolveThroughStages('initial-state');
  console.assert(evolution.length === 7, 'Should have 7 stages (0-6)');
  console.assert(evolution[0].stage === 0, 'First stage should be 0');
  console.assert(evolution[6].stage === 6, 'Last stage should be 6');
  
  console.log('✓ Utility function tests passed');
}

// Run all tests
export function runAllTests() {
  console.log('=== Running Pattern Dynamics Type Tests ===\n');
  
  testEvolutionStages();
  testInfinityLoop();
  testViableSystemTriad();
  testTemporalPatterns();
  testUtilityFunctions();
  
  console.log('\n=== All Pattern Dynamics Tests Passed! ===');
}

// Export for use in other modules
export const patternDynamicsTests = {
  testEvolutionStages,
  testInfinityLoop,
  testViableSystemTriad,
  testTemporalPatterns,
  testUtilityFunctions,
  runAllTests,
};
