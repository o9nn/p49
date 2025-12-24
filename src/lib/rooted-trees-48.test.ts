/**
 * rooted-trees-48.test.ts
 * 
 * Comprehensive test suite for rooted tree generation and Pattern Dynamics mapping
 */

import {
  treeToString,
  treeDepth,
  treeWidth,
  countNodes,
  classifyTreeShape,
  shapeToPatternFamily,
  getPatternMeaning,
  analyzeTree,
  generateRootedTrees,
  generateTreesWithConfig,
  generateComplete48TreeMapping,
  getSummaryStatistics,
  ONE_PLUS_FORTY_EIGHT,
  TREE_PATTERN_CORRESPONDENCES,
  type RootedTree,
} from './rooted-trees-48.data';

/**
 * Test basic tree utility functions
 */
export function testTreeUtilities() {
  console.log('Testing tree utility functions...');
  
  // Test empty tree (leaf node)
  const leaf: RootedTree = [];
  console.assert(treeToString(leaf) === '()', 'Leaf should be ()');
  console.assert(treeDepth(leaf) === 1, 'Leaf depth should be 1');
  console.assert(treeWidth(leaf) === 0, 'Leaf width should be 0');
  console.assert(countNodes(leaf) === 1, 'Leaf should have 1 node');
  
  // Test simple tree with one child
  const simple: RootedTree = [[]];
  console.assert(treeToString(simple) === '(())', 'Simple tree should be (())');
  console.assert(treeDepth(simple) === 2, 'Simple tree depth should be 2');
  console.assert(treeWidth(simple) === 1, 'Simple tree width should be 1');
  console.assert(countNodes(simple) === 2, 'Simple tree should have 2 nodes');
  
  // Test tree with multiple children
  const wide: RootedTree = [[], [], []];
  console.assert(
    treeToString(wide) === '(()()())',
    'Wide tree should be (()()())'
  );
  console.assert(treeDepth(wide) === 2, 'Wide tree depth should be 2');
  console.assert(treeWidth(wide) === 3, 'Wide tree width should be 3');
  console.assert(countNodes(wide) === 4, 'Wide tree should have 4 nodes');
  
  // Test deeply nested tree
  const deep: RootedTree = [[[[]]]] ;
  console.assert(
    treeToString(deep) === '((((())))',
    'Deep tree should be ((((()))))'
  );
  console.assert(treeDepth(deep) === 4, 'Deep tree depth should be 4');
  console.assert(treeWidth(deep) === 1, 'Deep tree width should be 1');
  console.assert(countNodes(deep) === 4, 'Deep tree should have 4 nodes');
  
  console.log('✓ Tree utility tests passed');
}

/**
 * Test tree shape classification
 */
export function testTreeShapeClassification() {
  console.log('Testing tree shape classification...');
  
  // Linear deep: depth >= 5, width <= 2
  console.assert(
    classifyTreeShape(6, 1) === 'linear-deep',
    'Deep narrow tree should be linear-deep'
  );
  console.assert(
    classifyTreeShape(5, 2) === 'linear-deep',
    'Deep tree with width 2 should be linear-deep'
  );
  
  // Wide balanced: depth <= 3, width >= 4
  console.assert(
    classifyTreeShape(2, 5) === 'wide-balanced',
    'Shallow wide tree should be wide-balanced'
  );
  console.assert(
    classifyTreeShape(3, 4) === 'wide-balanced',
    'Tree with depth 3, width 4 should be wide-balanced'
  );
  
  // Complex mixed: depth >= 4, width >= 3
  console.assert(
    classifyTreeShape(4, 3) === 'complex-mixed',
    'Tree with depth 4, width 3 should be complex-mixed'
  );
  console.assert(
    classifyTreeShape(5, 4) === 'complex-mixed',
    'Deep wide tree should be complex-mixed'
  );
  
  // Moderate: everything else
  console.assert(
    classifyTreeShape(3, 2) === 'moderate',
    'Moderate tree should be moderate'
  );
  console.assert(
    classifyTreeShape(4, 2) === 'moderate',
    'Mid-range tree should be moderate'
  );
  
  console.log('✓ Tree shape classification tests passed');
}

/**
 * Test pattern family mapping
 */
export function testPatternFamilyMapping() {
  console.log('Testing pattern family mapping...');
  
  console.assert(
    shapeToPatternFamily('linear-deep') === 'dynamics-family',
    'Linear-deep should map to dynamics-family'
  );
  console.assert(
    shapeToPatternFamily('wide-balanced') === 'source-family',
    'Wide-balanced should map to source-family'
  );
  console.assert(
    shapeToPatternFamily('complex-mixed') === 'structure-family',
    'Complex-mixed should map to structure-family'
  );
  console.assert(
    shapeToPatternFamily('moderate') === 'mixed-families',
    'Moderate should map to mixed-families'
  );
  
  console.log('✓ Pattern family mapping tests passed');
}

/**
 * Test pattern meaning retrieval
 */
export function testPatternMeaning() {
  console.log('Testing pattern meaning retrieval...');
  
  const linearMeaning = getPatternMeaning('linear-deep');
  console.assert(
    linearMeaning.includes('Sequential'),
    'Linear-deep meaning should mention Sequential'
  );
  console.assert(
    linearMeaning.includes('Dynamics'),
    'Linear-deep meaning should mention Dynamics'
  );
  
  const wideMeaning = getPatternMeaning('wide-balanced');
  console.assert(
    wideMeaning.includes('Simultaneous'),
    'Wide-balanced meaning should mention Simultaneous'
  );
  console.assert(
    wideMeaning.includes('Source'),
    'Wide-balanced meaning should mention Source'
  );
  
  const complexMeaning = getPatternMeaning('complex-mixed');
  console.assert(
    complexMeaning.includes('Holarchical'),
    'Complex-mixed meaning should mention Holarchical'
  );
  console.assert(
    complexMeaning.includes('Structure'),
    'Complex-mixed meaning should mention Structure'
  );
  
  console.log('✓ Pattern meaning tests passed');
}

/**
 * Test tree analysis
 */
export function testTreeAnalysis() {
  console.log('Testing tree analysis...');
  
  const tree: RootedTree = [[], [], []];
  const analysis = analyzeTree(tree, 1);
  
  console.assert(analysis.index === 1, 'Analysis should have correct index');
  console.assert(
    analysis.string === '(()()())',
    'Analysis should have correct string'
  );
  console.assert(analysis.depth === 2, 'Analysis should have correct depth');
  console.assert(analysis.width === 3, 'Analysis should have correct width');
  console.assert(analysis.nodes === 4, 'Analysis should have correct node count');
  console.assert(
    analysis.shape === 'moderate',
    'Analysis should have correct shape'
  );
  console.assert(
    analysis.patternFamily === 'mixed-families',
    'Analysis should have correct pattern family'
  );
  console.assert(
    typeof analysis.meaning === 'string' && analysis.meaning.length > 0,
    'Analysis should have a meaning'
  );
  
  console.log('✓ Tree analysis tests passed');
}

/**
 * Test tree generation for small values
 */
export function testSmallTreeGeneration() {
  console.log('Testing small tree generation...');
  
  // n=1: Only one tree (empty)
  const trees1 = generateRootedTrees(1);
  console.assert(trees1.length === 1, 'n=1 should have 1 tree');
  console.assert(
    treeToString(trees1[0]) === '()',
    'n=1 tree should be ()'
  );
  
  // n=2: Only one tree
  const trees2 = generateRootedTrees(2);
  console.assert(trees2.length === 1, 'n=2 should have 1 tree');
  console.assert(
    treeToString(trees2[0]) === '(())',
    'n=2 tree should be (())'
  );
  
  // n=3: Two trees
  const trees3 = generateRootedTrees(3);
  console.assert(trees3.length === 2, 'n=3 should have 2 trees');
  
  // n=4: Four trees
  const trees4 = generateRootedTrees(4);
  console.assert(trees4.length === 4, 'n=4 should have 4 trees');
  
  console.log('✓ Small tree generation tests passed');
}

/**
 * Test OEIS A000081 sequence validation
 */
export function testOEISSequence() {
  console.log('Testing OEIS A000081 sequence...');
  
  // Known values from OEIS A000081
  const expected = [
    { n: 1, count: 1 },
    { n: 2, count: 1 },
    { n: 3, count: 2 },
    { n: 4, count: 4 },
    { n: 5, count: 9 },
    { n: 6, count: 20 },
    { n: 7, count: 48 },
  ];
  
  const cache = {};
  for (const { n, count } of expected) {
    const trees = generateRootedTrees(n, cache);
    console.assert(
      trees.length === count,
      `n=${n} should generate ${count} trees, got ${trees.length}`
    );
  }
  
  console.log('✓ OEIS A000081 sequence validation passed');
}

/**
 * Test the fundamental 48 trees generation
 */
export function testFortyEightTrees() {
  console.log('Testing 48-tree generation...');
  
  const result = generateTreesWithConfig({
    n: 7,
    useMemoization: true,
    includeAnalysis: true,
  });
  
  console.assert(
    result.count === 48,
    `Should generate exactly 48 trees for n=7, got ${result.count}`
  );
  console.assert(result.trees.length === 48, 'Should have 48 tree structures');
  console.assert(
    result.analyses && result.analyses.length === 48,
    'Should have 48 tree analyses'
  );
  console.assert(
    result.generationTime >= 0,
    'Generation time should be non-negative'
  );
  
  // Verify all trees have 7 nodes
  for (let i = 0; i < result.trees.length; i++) {
    const nodeCount = countNodes(result.trees[i]);
    console.assert(
      nodeCount === 7,
      `Tree ${i + 1} should have 7 nodes, got ${nodeCount}`
    );
  }
  
  // Verify all analyses have correct indices
  if (result.analyses) {
    for (let i = 0; i < result.analyses.length; i++) {
      console.assert(
        result.analyses[i].index === i + 1,
        `Analysis ${i} should have index ${i + 1}`
      );
    }
  }
  
  console.log('✓ 48-tree generation tests passed');
}

/**
 * Test tree generation with configuration options
 */
export function testGenerationConfig() {
  console.log('Testing generation configuration...');
  
  // Test with maxTrees limit
  const limited = generateTreesWithConfig({
    n: 7,
    maxTrees: 10,
    includeAnalysis: false,
  });
  console.assert(
    limited.count === 10,
    'Should limit to 10 trees'
  );
  console.assert(
    limited.analyses === undefined,
    'Should not include analyses when disabled'
  );
  
  // Test without memoization
  const noCache = generateTreesWithConfig({
    n: 5,
    useMemoization: false,
    includeAnalysis: true,
  });
  console.assert(
    noCache.count === 9,
    'Should generate 9 trees for n=5 without cache'
  );
  
  console.log('✓ Generation configuration tests passed');
}

/**
 * Test tree uniqueness (no duplicates)
 */
export function testTreeUniqueness() {
  console.log('Testing tree uniqueness...');
  
  const result = generateTreesWithConfig({
    n: 7,
    useMemoization: true,
    includeAnalysis: true,
  });
  
  const stringSet = new Set<string>();
  for (const analysis of result.analyses!) {
    const str = analysis.string;
    console.assert(
      !stringSet.has(str),
      `Duplicate tree found: ${str}`
    );
    stringSet.add(str);
  }
  
  console.assert(
    stringSet.size === 48,
    'Should have 48 unique tree strings'
  );
  
  console.log('✓ Tree uniqueness tests passed');
}

/**
 * Test complete 48-tree mapping structure
 */
export function testComplete48Mapping() {
  console.log('Testing complete 48-tree mapping...');
  
  const mapping = generateComplete48TreeMapping();
  
  // Test structure
  console.assert(
    mapping.structure === ONE_PLUS_FORTY_EIGHT,
    'Should include fundamental structure'
  );
  console.assert(
    mapping.structure.oeis.fortyEight.includes('48'),
    'OEIS description should mention 48'
  );
  console.assert(
    mapping.structure.patterns.fortyEight.includes('48'),
    'Pattern description should mention 48'
  );
  
  // Test trees
  console.assert(
    mapping.trees.length === 48,
    'Should have 48 tree analyses'
  );
  
  // Test correspondences
  console.assert(
    mapping.correspondences === TREE_PATTERN_CORRESPONDENCES,
    'Should include correspondences'
  );
  console.assert(
    mapping.correspondences.length >= 10,
    'Should have at least 10 correspondences'
  );
  
  // Test statistics
  console.assert(
    mapping.statistics.length > 0,
    'Should have shape statistics'
  );
  
  const totalCount = mapping.statistics.reduce(
    (sum, stat) => sum + stat.count,
    0
  );
  console.assert(
    totalCount === 48,
    'Statistics should sum to 48 trees'
  );
  
  // Verify percentages sum to 100 (with floating point tolerance)
  const totalPercentage = mapping.statistics.reduce(
    (sum, stat) => sum + stat.percentage,
    0
  );
  console.assert(
    Math.abs(totalPercentage - 100) < 0.01,
    'Percentages should sum to 100%'
  );
  
  // Test mappings
  console.assert(
    mapping.mappings.length > 0,
    'Should have tree-to-pattern mappings'
  );
  
  console.log('✓ Complete 48-tree mapping tests passed');
}

/**
 * Test summary statistics
 */
export function testSummaryStatistics() {
  console.log('Testing summary statistics...');
  
  const result = generateTreesWithConfig({
    n: 7,
    useMemoization: true,
    includeAnalysis: true,
  });
  
  const stats = getSummaryStatistics(result.analyses!);
  
  console.assert(stats.total === 48, 'Total should be 48');
  console.assert(
    typeof stats.byShape === 'object',
    'Should have shape breakdown'
  );
  console.assert(
    typeof stats.byFamily === 'object',
    'Should have family breakdown'
  );
  
  // Check depth statistics
  console.assert(
    stats.depths.min >= 1,
    'Minimum depth should be at least 1'
  );
  console.assert(
    stats.depths.max >= stats.depths.min,
    'Maximum depth should be >= minimum'
  );
  console.assert(
    stats.depths.avg >= stats.depths.min &&
      stats.depths.avg <= stats.depths.max,
    'Average depth should be between min and max'
  );
  
  // Check width statistics
  console.assert(
    stats.widths.min >= 0,
    'Minimum width should be non-negative'
  );
  console.assert(
    stats.widths.max >= stats.widths.min,
    'Maximum width should be >= minimum'
  );
  console.assert(
    stats.widths.avg >= stats.widths.min &&
      stats.widths.avg <= stats.widths.max,
    'Average width should be between min and max'
  );
  
  console.log('✓ Summary statistics tests passed');
}

/**
 * Test the 1+48=49 correspondence structure
 */
export function testOnePlusFortyEightStructure() {
  console.log('Testing 1+48=49 structure...');
  
  const structure = ONE_PLUS_FORTY_EIGHT;
  
  // OEIS perspective
  console.assert(
    structure.oeis.one.includes('1'),
    'OEIS one should mention 1'
  );
  console.assert(
    structure.oeis.fortyEight.includes('48'),
    'OEIS fortyEight should mention 48'
  );
  console.assert(
    structure.oeis.total.includes('49'),
    'OEIS total should mention 49'
  );
  
  // Pattern Dynamics perspective
  console.assert(
    structure.patterns.one.includes('Source'),
    'Patterns one should mention Source'
  );
  console.assert(
    structure.patterns.fortyEight.includes('48'),
    'Patterns fortyEight should mention 48'
  );
  console.assert(
    structure.patterns.total.includes('49'),
    'Patterns total should mention 49'
  );
  
  // Correspondence
  console.assert(
    structure.correspondence.includes('1+48=49'),
    'Correspondence should mention 1+48=49'
  );
  console.assert(
    structure.correspondence.includes('unity'),
    'Correspondence should mention unity'
  );
  console.assert(
    structure.correspondence.includes('multiplicity'),
    'Correspondence should mention multiplicity'
  );
  
  console.log('✓ 1+48=49 structure tests passed');
}

/**
 * Test tree-pattern correspondences
 */
export function testTreePatternCorrespondences() {
  console.log('Testing tree-pattern correspondences...');
  
  const correspondences = TREE_PATTERN_CORRESPONDENCES;
  
  console.assert(
    correspondences.length >= 10,
    'Should have at least 10 correspondences'
  );
  
  // Check that all correspondences have required fields
  for (const corr of correspondences) {
    console.assert(
      typeof corr.treeConcept === 'string' && corr.treeConcept.length > 0,
      'Each correspondence should have a tree concept'
    );
    console.assert(
      typeof corr.patternMeaning === 'string' && corr.patternMeaning.length > 0,
      'Each correspondence should have a pattern meaning'
    );
    console.assert(
      typeof corr.explanation === 'string' && corr.explanation.length > 0,
      'Each correspondence should have an explanation'
    );
  }
  
  // Check for key concepts
  const concepts = correspondences.map((c) => c.treeConcept);
  console.assert(
    concepts.some((c) => c.includes('Root')),
    'Should have root node correspondence'
  );
  console.assert(
    concepts.some((c) => c.includes('depth') || c.includes('Depth')),
    'Should have depth correspondence'
  );
  console.assert(
    concepts.some((c) => c.includes('Recursion') || c.includes('recursion')),
    'Should have recursion correspondence'
  );
  
  console.log('✓ Tree-pattern correspondences tests passed');
}

/**
 * Run all tests
 */
export function runAllRootedTreeTests() {
  console.log('\n========================================');
  console.log('Running Rooted Trees 48 Test Suite');
  console.log('========================================\n');
  
  try {
    testTreeUtilities();
    testTreeShapeClassification();
    testPatternFamilyMapping();
    testPatternMeaning();
    testTreeAnalysis();
    testSmallTreeGeneration();
    testOEISSequence();
    testFortyEightTrees();
    testGenerationConfig();
    testTreeUniqueness();
    testComplete48Mapping();
    testSummaryStatistics();
    testOnePlusFortyEightStructure();
    testTreePatternCorrespondences();
    
    console.log('\n========================================');
    console.log('✓ All Rooted Trees 48 Tests Passed!');
    console.log('========================================\n');
    
    return true;
  } catch (error) {
    console.error('\n✗ Test failed with error:', error);
    return false;
  }
}

// Allow running tests directly
if (typeof window !== 'undefined') {
  (window as any).runAllRootedTreeTests = runAllRootedTreeTests;
}
