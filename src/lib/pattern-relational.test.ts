/**
 * pattern-relational.test.ts
 * Tests for Pattern Dynamics relational expressions and holarchies
 */

import {
  CORE_PATTERNS,
  SET_OPERATORS,
  getPatternAspect,
  getAllPatternRelationships,
  evaluateRelation,
  composePatterns,
  type CorePattern
} from './pattern-relational.types';

import {
  PATTERN_MATRIX,
  GLOBAL_PROCESSES,
  HOLARCHICAL_RELATIONS,
  META_PATTERNS,
  RELATIONAL_PRINCIPLES
} from './pattern-relational.data';

// Test core patterns
export function testCorePatterns() {
  console.log('Testing Core Patterns...');
  
  console.assert(CORE_PATTERNS.length === 7, 'Should have 7 core patterns');
  console.assert(CORE_PATTERNS.includes('source'), 'Should include source');
  console.assert(CORE_PATTERNS.includes('dynamics'), 'Should include dynamics');
  console.assert(CORE_PATTERNS.includes('creative'), 'Should include creative');
  console.assert(CORE_PATTERNS.includes('exchange'), 'Should include exchange');
  console.assert(CORE_PATTERNS.includes('structure'), 'Should include structure');
  console.assert(CORE_PATTERNS.includes('polarity'), 'Should include polarity');
  console.assert(CORE_PATTERNS.includes('rhythm'), 'Should include rhythm');
  
  console.log('✓ Core patterns tests passed');
}

// Test set operators
export function testSetOperators() {
  console.log('Testing Set Operators...');
  
  const operators = Object.keys(SET_OPERATORS);
  console.assert(operators.length === 7, 'Should have 7 set operators');
  
  console.assert(SET_OPERATORS['∈'].semanticMeaning === 'IS', 'IS operator correct');
  console.assert(SET_OPERATORS['⊂'].semanticMeaning === 'Dynamics', 'Dynamics operator correct');
  console.assert(SET_OPERATORS['∩'].semanticMeaning === 'OR', 'OR operator correct');
  console.assert(SET_OPERATORS['⊆'].semanticMeaning === 'Exchange', 'Exchange operator correct');
  console.assert(SET_OPERATORS['∪'].semanticMeaning === 'Structure', 'Structure operator correct');
  console.assert(SET_OPERATORS['∉'].semanticMeaning === 'NOT', 'NOT operator correct');
  console.assert(SET_OPERATORS['∅'].semanticMeaning === 'EMPTY', 'EMPTY operator correct');
  
  console.log('✓ Set operators tests passed');
}

// Test pattern matrix
export function testPatternMatrix() {
  console.log('Testing Pattern Matrix...');
  
  // Test matrix dimensions
  const rows = Object.keys(PATTERN_MATRIX);
  console.assert(rows.length === 7, 'Matrix should have 7 rows');
  
  // Test each row has 7 columns
  rows.forEach(row => {
    const cols = Object.keys(PATTERN_MATRIX[row as CorePattern]);
    console.assert(cols.length === 7, `Row ${row} should have 7 columns`);
  });
  
  // Test specific relationships
  const sourceToDynamics = PATTERN_MATRIX.source.dynamics;
  console.assert(sourceToDynamics.relationship === 'Unity Dynamics', 'Source-Dynamics relationship correct');
  console.assert(sourceToDynamics.cellNumber === 40, 'Cell 40 reference correct');
  
  const dynamicsToDynamics = PATTERN_MATRIX.dynamics.dynamics;
  console.assert(dynamicsToDynamics.cellNumber === 42, 'Cell 42 (Integrated Dynamics) correct');
  
  const creativeToSource = PATTERN_MATRIX.creative.source;
  console.assert(creativeToSource.cellNumber === 41, 'Cell 41 (Self Creation) correct');
  
  console.log('✓ Pattern matrix tests passed');
}

// Test global processes
export function testGlobalProcesses() {
  console.log('Testing Global Processes...');
  
  console.assert(GLOBAL_PROCESSES.length === 8, 'Should have 8 global processes');
  
  // Test numbered processes
  const process17 = GLOBAL_PROCESSES.find(p => p.number === 17);
  console.assert(process17?.type === 'distinction', 'Process 17 is distinction');
  
  const process23 = GLOBAL_PROCESSES.find(p => p.number === 23);
  console.assert(process23?.type === 'unfolding', 'Process 23 is unfolding');
  
  const process39 = GLOBAL_PROCESSES.find(p => p.number === 39);
  console.assert(process39?.scope === 'local', 'Process 39 is local');
  
  const process40 = GLOBAL_PROCESSES.find(p => p.number === 40);
  console.assert(process40?.scope === 'global', 'Process 40 is global');
  
  const process41 = GLOBAL_PROCESSES.find(p => p.number === 41);
  console.assert(process41?.mechanism.includes('autopoiesis'), 'Process 41 includes autopoiesis');
  
  console.log('✓ Global processes tests passed');
}

// Test holarchical relations
export function testHolarchicalRelations() {
  console.log('Testing Holarchical Relations...');
  
  console.assert(HOLARCHICAL_RELATIONS.length >= 9, 'Should have multiple holarchical relations');
  
  // Test emergence from source
  const dynamicsFromSource = HOLARCHICAL_RELATIONS.find(
    r => r.parent === 'dynamics' && r.child === 'source'
  );
  console.assert(dynamicsFromSource?.type === 'emerges-from', 'Dynamics emerges from Source');
  
  // Test bidirectional relations
  const bidirectional = HOLARCHICAL_RELATIONS.filter(r => r.bidirectional);
  console.assert(bidirectional.length >= 1, 'Should have bidirectional relations');
  
  console.log('✓ Holarchical relations tests passed');
}

// Test meta-patterns
export function testMetaPatterns() {
  console.log('Testing Meta-Patterns...');
  
  console.assert(META_PATTERNS.length === 6, 'Should have 6 meta-patterns');
  
  // Test Unity Field
  const unityField = META_PATTERNS.find(m => m.name === 'Unity Field');
  console.assert(unityField !== undefined, 'Unity Field exists');
  console.assert(
    unityField?.composedFrom.includes('source') && unityField?.composedFrom.includes('rhythm'),
    'Unity Field composed from source and rhythm'
  );
  
  // Test Transcendent Holarchy
  const transcendent = META_PATTERNS.find(m => m.name === 'Transcendent Holarchy');
  console.assert(transcendent !== undefined, 'Transcendent Holarchy exists');
  console.assert(transcendent?.composedFrom.length === 7, 'Transcendent Holarchy includes all 7 patterns');
  
  console.log('✓ Meta-patterns tests passed');
}

// Test relational principles
export function testRelationalPrinciples() {
  console.log('Testing Relational Principles...');
  
  console.assert(RELATIONAL_PRINCIPLES.length === 5, 'Should have 5 relational principles');
  
  // Test Dynamic Stability principle
  const dynamicStability = RELATIONAL_PRINCIPLES.find(p => p.name === 'Dynamic Stability');
  console.assert(dynamicStability !== undefined, 'Dynamic Stability principle exists');
  console.assert(
    dynamicStability?.patterns.includes('dynamics') && 
    dynamicStability?.patterns.includes('polarity'),
    'Dynamic Stability includes dynamics and polarity'
  );
  
  console.log('✓ Relational principles tests passed');
}

// Test pattern aspect retrieval
export function testPatternAspects() {
  console.log('Testing Pattern Aspects...');
  
  const sourceToSource = getPatternAspect('source', 'source');
  console.assert(sourceToSource !== undefined, 'Source-Source aspect exists');
  console.assert(sourceToSource?.aspectName === 'Ground Source', 'Source-Source aspect name correct');
  
  const sourceToDynamics = getPatternAspect('source', 'dynamics');
  console.assert(sourceToDynamics?.aspectName === 'Unity Dynamics', 'Source-Dynamics aspect correct');
  
  const dynamicsToCreative = getPatternAspect('dynamics', 'creative');
  console.assert(dynamicsToCreative?.aspectName === 'Dynamic Creativity', 'Dynamics-Creative aspect correct');
  
  console.log('✓ Pattern aspects tests passed');
}

// Test all pattern relationships
export function testAllPatternRelationships() {
  console.log('Testing All Pattern Relationships...');
  
  const sourceRelationships = getAllPatternRelationships('source');
  console.assert(sourceRelationships.length === 7, 'Source has 7 relationships');
  
  const dynamicsRelationships = getAllPatternRelationships('dynamics');
  console.assert(dynamicsRelationships.length === 7, 'Dynamics has 7 relationships');
  
  console.log('✓ All pattern relationships tests passed');
}

// Test relational expression evaluation
export function testRelationalEvaluation() {
  console.log('Testing Relational Expression Evaluation...');
  
  const expr1 = evaluateRelation('source', '⊂', 'dynamics');
  console.assert(expr1.includes('source'), 'Expression includes source');
  console.assert(expr1.includes('⊂'), 'Expression includes operator');
  console.assert(expr1.includes('dynamics'), 'Expression includes dynamics');
  
  console.log('✓ Relational evaluation tests passed');
}

// Test pattern composition
export function testPatternComposition() {
  console.log('Testing Pattern Composition...');
  
  const composed = composePatterns('source', 'rhythm');
  console.assert(composed.from === 'source', 'Composition from is correct');
  console.assert(composed.to === 'rhythm', 'Composition to is correct');
  console.assert(composed.relationship === 'composition', 'Relationship type is composition');
  
  console.log('✓ Pattern composition tests passed');
}

// Run all tests
export function runAllRelationalTests() {
  console.log('\n=== Running Pattern Relational Tests ===\n');
  
  testCorePatterns();
  testSetOperators();
  testPatternMatrix();
  testGlobalProcesses();
  testHolarchicalRelations();
  testMetaPatterns();
  testRelationalPrinciples();
  testPatternAspects();
  testAllPatternRelationships();
  testRelationalEvaluation();
  testPatternComposition();
  
  console.log('\n=== All Pattern Relational Tests Passed ✓ ===\n');
}

// Auto-run tests if this module is executed directly
if (typeof window === 'undefined') {
  runAllRelationalTests();
}
