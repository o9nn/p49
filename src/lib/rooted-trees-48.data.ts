/**
 * rooted-trees-48.data.ts
 * 
 * Implementation of rooted tree generation and Pattern Dynamics mapping
 */

import type {
  RootedTree,
  TreeString,
  TreeShape,
  PatternFamily,
  TreeAnalysis,
  TreePatternCorrespondence,
  OnePlusFortyEightStructure,
  TreeGenerationConfig,
  TreeGenerationResult,
  TreeShapeStatistics,
  Complete48TreeMapping,
} from './rooted-trees-48.types';

/**
 * Convert tree structure to parentheses notation
 */
export function treeToString(tree: RootedTree): TreeString {
  if (tree.length === 0) {
    return '()';
  }
  return '(' + tree.map(treeToString).join('') + ')';
}

/**
 * Calculate maximum depth of tree
 */
export function treeDepth(tree: RootedTree): number {
  if (tree.length === 0) {
    return 1;
  }
  return 1 + Math.max(...tree.map(treeDepth));
}

/**
 * Calculate width (number of children at root)
 */
export function treeWidth(tree: RootedTree): number {
  return tree.length;
}

/**
 * Count total nodes in tree
 */
export function countNodes(tree: RootedTree): number {
  if (tree.length === 0) {
    return 1;
  }
  return 1 + tree.reduce((sum, subtree) => sum + countNodes(subtree), 0);
}

/**
 * Classify tree shape based on depth and width
 */
export function classifyTreeShape(depth: number, width: number): TreeShape {
  if (depth >= 5 && width <= 2) {
    return 'linear-deep';
  } else if (depth <= 3 && width >= 4) {
    return 'wide-balanced';
  } else if (depth >= 4 && width >= 3) {
    return 'complex-mixed';
  } else {
    return 'moderate';
  }
}

/**
 * Map tree shape to Pattern Dynamics family
 */
export function shapeToPatternFamily(shape: TreeShape): PatternFamily {
  const mapping: Record<TreeShape, PatternFamily> = {
    'linear-deep': 'dynamics-family',
    'wide-balanced': 'source-family',
    'complex-mixed': 'structure-family',
    'moderate': 'mixed-families',
  };
  return mapping[shape];
}

/**
 * Get pattern meaning for tree shape
 */
export function getPatternMeaning(shape: TreeShape): string {
  const meanings: Record<TreeShape, string> = {
    'linear-deep': 
      'Sequential emergence: Linear developmental path with maximum ' +
      'differentiation and depth. Like Dynamics family patterns that ' +
      'unfold through feedback loops and iterative processes.',
    'wide-balanced':
      'Simultaneous emergence: Parallel differentiation with horizontal ' +
      'diversity. Like Source family patterns that manifest multiple ' +
      'aspects at once from unified origin.',
    'complex-mixed':
      'Holarchical integration: Balanced nesting and branching creating ' +
      'integrated complexity. Like Structure family patterns organizing ' +
      'hierarchies and networks.',
    'moderate':
      'Complex emergence: Mixed patterns of nesting and branching ' +
      'representing multi-level holarchical relationships.',
  };
  return meanings[shape];
}

/**
 * Analyze a rooted tree
 */
export function analyzeTree(tree: RootedTree, index: number): TreeAnalysis {
  const depth = treeDepth(tree);
  const width = treeWidth(tree);
  const shape = classifyTreeShape(depth, width);
  
  return {
    index,
    tree,
    string: treeToString(tree),
    depth,
    width,
    nodes: countNodes(tree),
    shape,
    patternFamily: shapeToPatternFamily(shape),
    meaning: getPatternMeaning(shape),
  };
}

/**
 * Generate all partitions of n in non-increasing order
 */
function partitions(n: number, maxVal: number = n): number[][] {
  const result: number[][] = [];
  
  function generate(remaining: number, max: number, current: number[]): void {
    if (remaining === 0) {
      result.push([...current]);
      return;
    }
    
    for (let i = Math.min(remaining, max); i >= 1; i--) {
      current.push(i);
      generate(remaining - i, i, current);
      current.pop();
    }
  }
  
  generate(n, maxVal, []);
  return result;
}

/**
 * Generate k-element multiset combinations
 */
function multisetCombinations<T>(items: T[], k: number): T[][] {
  const result: T[][] = [];
  
  function generate(remaining: number, startIdx: number, current: T[]): void {
    if (remaining === 0) {
      result.push([...current]);
      return;
    }
    if (startIdx >= items.length) {
      return;
    }
    
    // Include current item
    current.push(items[startIdx]);
    generate(remaining - 1, startIdx, current);
    current.pop();
    
    // Move to next item
    generate(remaining, startIdx + 1, current);
  }
  
  generate(k, 0, []);
  return result;
}

/**
 * Generate trees from a partition
 */
function treesFromPartition(
  partition: number[],
  cache: { [key: number]: RootedTree[] }
): RootedTree[] {
  if (partition.length === 0) {
    return [[]];
  }
  
  // Count occurrences of each size
  const sizeCounts: { [key: number]: number } = {};
  for (const size of partition) {
    sizeCounts[size] = (sizeCounts[size] || 0) + 1;
  }
  
  // Generate combinations recursively
  const sizes = Object.keys(sizeCounts).map(Number).sort((a, b) => b - a);
  
  function generateCombos(
    sizesLeft: number[],
    counts: { [key: number]: number }
  ): RootedTree[] {
    if (sizesLeft.length === 0) {
      return [[]];
    }
    
    const size = sizesLeft[0];
    const count = counts[size];
    const trees = cache[size] || [];
    
    const results: RootedTree[] = [];
    for (const selection of multisetCombinations(trees, count)) {
      for (const rest of generateCombos(sizesLeft.slice(1), counts)) {
        results.push([...selection, ...rest]);
      }
    }
    return results;
  }
  
  return generateCombos(sizes, sizeCounts);
}

/**
 * Generate all rooted trees with n nodes
 */
export function generateRootedTrees(
  n: number,
  cache: { [key: number]: RootedTree[] } = {}
): RootedTree[] {
  if (cache[n]) {
    return cache[n];
  }
  
  if (n === 0) {
    return [];
  }
  if (n === 1) {
    const result = [[]];
    cache[n] = result;
    return result;
  }
  
  // Generate all smaller trees first
  for (let i = 1; i < n; i++) {
    if (!cache[i]) {
      generateRootedTrees(i, cache);
    }
  }
  
  const trees: RootedTree[] = [];
  
  for (const partition of partitions(n - 1)) {
    for (const treeCombo of treesFromPartition(partition, cache)) {
      trees.push(treeCombo);
    }
  }
  
  cache[n] = trees;
  return trees;
}

/**
 * Generate trees with configuration
 */
export function generateTreesWithConfig(
  config: TreeGenerationConfig
): TreeGenerationResult {
  const startTime = performance.now();
  const cache = config.useMemoization !== false ? {} : undefined;
  
  let trees = generateRootedTrees(config.n, cache);
  
  if (config.maxTrees && trees.length > config.maxTrees) {
    trees = trees.slice(0, config.maxTrees);
  }
  
  const analyses = config.includeAnalysis
    ? trees.map((tree, idx) => analyzeTree(tree, idx + 1))
    : undefined;
  
  const endTime = performance.now();
  
  return {
    count: trees.length,
    trees,
    analyses,
    generationTime: endTime - startTime,
  };
}

/**
 * The fundamental 1+48 structure
 */
export const ONE_PLUS_FORTY_EIGHT: OnePlusFortyEightStructure = {
  oeis: {
    one: '1 root node',
    fortyEight: '48 rooted trees with 7 nodes',
    total: '49 total structures (1 root position + 48 distinct arrangements)',
  },
  patterns: {
    one: '1 Source pattern (NonDual Origin)',
    fortyEight: '48 emergent patterns (6 first-order + 42 second-order)',
    total: '49 total patterns in the holarchy',
  },
  correspondence:
    'Both systems exhibit a 1+48=49 structure representing unity ' +
    'differentiating into multiplicity while maintaining wholeness. ' +
    'The 48 trees correspond to the 48 ways patterns can emerge from Source.',
};

/**
 * Conceptual correspondences between trees and patterns
 */
export const TREE_PATTERN_CORRESPONDENCES: TreePatternCorrespondence[] = [
  {
    treeConcept: 'Root node',
    patternMeaning: 'Source pattern (NonDual Origin)',
    explanation:
      'The root represents the undifferentiated source from which all complexity emerges',
  },
  {
    treeConcept: 'Child nodes',
    patternMeaning: 'Emergent patterns from Source',
    explanation: 'Each child represents a differentiation or aspect that emerges',
  },
  {
    treeConcept: 'Tree depth / Nesting levels',
    patternMeaning: 'Pattern order (0th, 1st, 2nd)',
    explanation:
      'Depth represents the degree of differentiation and emergence from Source',
  },
  {
    treeConcept: 'Branching factor',
    patternMeaning: 'Holarchical crossing / differentiation',
    explanation:
      'Multiple branches represent simultaneous emergence of different aspects',
  },
  {
    treeConcept: 'Nested subtrees',
    patternMeaning: 'Pattern containment and emergence',
    explanation: 'Subtrees contain the same structure, mirroring pattern recursion',
  },
  {
    treeConcept: 'Parentheses pairs ()',
    patternMeaning: 'Pattern domain boundaries',
    explanation: 'Parentheses delimit the scope and extent of each pattern',
  },
  {
    treeConcept: 'Recursion (tree of trees)',
    patternMeaning: 'Self-similar structure at each level',
    explanation:
      'Both systems exhibit the same structural principle at every scale',
  },
  {
    treeConcept: 'Sibling subtrees',
    patternMeaning: 'Co-emergent patterns (same order)',
    explanation: 'Siblings represent patterns that emerge at the same level',
  },
  {
    treeConcept: 'Path from root to node',
    patternMeaning: 'Genealogy of pattern emergence',
    explanation: 'The path traces how a pattern emerged through various crossings',
  },
  {
    treeConcept: 'Leaf nodes',
    patternMeaning: 'Fully differentiated pattern instances',
    explanation: 'Leaves represent patterns that have reached full expression',
  },
];

/**
 * Generate complete 48-tree mapping
 */
export function generateComplete48TreeMapping(): Complete48TreeMapping {
  const result = generateTreesWithConfig({
    n: 7,
    useMemoization: true,
    includeAnalysis: true,
  });
  
  const analyses = result.analyses!;
  
  // Calculate statistics by shape
  const shapeGroups: { [key: string]: TreeAnalysis[] } = {};
  for (const analysis of analyses) {
    const shape = analysis.shape;
    if (!shapeGroups[shape]) {
      shapeGroups[shape] = [];
    }
    shapeGroups[shape].push(analysis);
  }
  
  const statistics: TreeShapeStatistics[] = Object.keys(shapeGroups).map(
    (shapeKey) => {
      const shape = shapeKey as TreeShape;
      const group = shapeGroups[shapeKey];
      return {
        shape,
        count: group.length,
        percentage: (group.length / analyses.length) * 100,
        patternFamily: shapeToPatternFamily(shape),
        examples: group.slice(0, 5).map((a) => a.string),
      };
    }
  );
  
  // Create simplified mappings (full mapping would be very large)
  const mappings = analyses.slice(0, 10).map((tree) => ({
    treeIndex: tree.index,
    tree,
    pattern: {
      name: `Pattern-${tree.index}`,
      order: (tree.depth <= 2 ? 0 : tree.depth <= 4 ? 1 : 2) as 0 | 1 | 2,
      family: tree.patternFamily,
      description: tree.meaning,
    },
    rationale: `Tree ${tree.index} with ${tree.shape} structure maps to ${tree.patternFamily}`,
  }));
  
  return {
    structure: ONE_PLUS_FORTY_EIGHT,
    trees: analyses,
    correspondences: TREE_PATTERN_CORRESPONDENCES,
    statistics,
    mappings,
  };
}

/**
 * Get summary statistics
 */
export function getSummaryStatistics(analyses: TreeAnalysis[]) {
  const byShape: { [key: string]: number } = {};
  const byFamily: { [key: string]: number } = {};
  
  for (const analysis of analyses) {
    byShape[analysis.shape] = (byShape[analysis.shape] || 0) + 1;
    byFamily[analysis.patternFamily] = (byFamily[analysis.patternFamily] || 0) + 1;
  }
  
  return {
    total: analyses.length,
    byShape,
    byFamily,
    depths: {
      min: Math.min(...analyses.map((a) => a.depth)),
      max: Math.max(...analyses.map((a) => a.depth)),
      avg: analyses.reduce((sum, a) => sum + a.depth, 0) / analyses.length,
    },
    widths: {
      min: Math.min(...analyses.map((a) => a.width)),
      max: Math.max(...analyses.map((a) => a.width)),
      avg: analyses.reduce((sum, a) => sum + a.width, 0) / analyses.length,
    },
  };
}
