/**
 * rooted-trees-48.types.ts
 * 
 * TypeScript types for mapping OEIS A000081 rooted trees
 * to Pattern Dynamics 49 patterns
 */

/**
 * A rooted tree represented as nested arrays
 * Empty array [] represents a leaf node
 * [subtree1, subtree2, ...] represents a node with children
 */
export type RootedTree = RootedTree[];

/**
 * Tree in parentheses notation, e.g., "((()))"
 */
export type TreeString = string;

/**
 * Shape classification for trees
 */
export type TreeShape = 
  | 'linear-deep'      // Maximum depth, minimal branching
  | 'wide-balanced'    // Minimal depth, maximum branching
  | 'complex-mixed'    // Balanced depth and branching
  | 'moderate';        // In between

/**
 * Pattern Dynamics family that tree corresponds to
 */
export type PatternFamily =
  | 'source-family'
  | 'dynamics-family'
  | 'creativity-family'
  | 'exchange-family'
  | 'structure-family'
  | 'polarity-family'
  | 'rhythm-family'
  | 'mixed-families';

/**
 * Analysis of a rooted tree
 */
export interface TreeAnalysis {
  /** Index in enumeration (1-48) */
  index: number;
  
  /** Tree structure */
  tree: RootedTree;
  
  /** Parentheses notation */
  string: TreeString;
  
  /** Maximum depth (nesting level) */
  depth: number;
  
  /** Width (number of children at root) */
  width: number;
  
  /** Total number of nodes */
  nodes: number;
  
  /** Shape classification */
  shape: TreeShape;
  
  /** Corresponding Pattern Dynamics family */
  patternFamily: PatternFamily;
  
  /** Semantic meaning in Pattern Dynamics context */
  meaning: string;
}

/**
 * Correspondence between tree structures and pattern meanings
 */
export interface TreePatternCorrespondence {
  /** Tree concept */
  treeConcept: string;
  
  /** Pattern Dynamics meaning */
  patternMeaning: string;
  
  /** Explanation */
  explanation: string;
}

/**
 * The fundamental 1+48=49 structure
 */
export interface OnePlusFortyEightStructure {
  /** OEIS A000081 perspective */
  oeis: {
    one: string;  // "1 root node"
    fortyEight: string;  // "48 rooted trees with 7 nodes"
    total: string;  // "49 total nodes across all structures"
  };
  
  /** Pattern Dynamics perspective */
  patterns: {
    one: string;  // "1 Source pattern"
    fortyEight: string;  // "48 emergent patterns (6+42)"
    total: string;  // "49 total patterns"
  };
  
  /** The isomorphism */
  correspondence: string;
}

/**
 * Configuration for tree generation and mapping
 */
export interface TreeGenerationConfig {
  /** Number of nodes (default: 7 for a(7)=48) */
  n: number;
  
  /** Use memoization for efficiency */
  useMemoization?: boolean;
  
  /** Maximum trees to generate (for limiting output) */
  maxTrees?: number;
  
  /** Include detailed analysis */
  includeAnalysis?: boolean;
}

/**
 * Result of tree generation
 */
export interface TreeGenerationResult {
  /** Total number of trees generated */
  count: number;
  
  /** All generated trees */
  trees: RootedTree[];
  
  /** Analyses (if includeAnalysis was true) */
  analyses?: TreeAnalysis[];
  
  /** Generation time in milliseconds */
  generationTime: number;
}

/**
 * Mapping between a specific tree and pattern
 */
export interface TreeToPatternMapping {
  /** Tree index (1-48) */
  treeIndex: number;
  
  /** Tree structure */
  tree: TreeAnalysis;
  
  /** Mapped pattern (from Pattern Dynamics 49) */
  pattern: {
    name: string;
    order: 0 | 1 | 2;
    family: PatternFamily;
    description: string;
  };
  
  /** Explanation of correspondence */
  rationale: string;
}

/**
 * Statistical summary of tree shapes
 */
export interface TreeShapeStatistics {
  shape: TreeShape;
  count: number;
  percentage: number;
  patternFamily: PatternFamily;
  examples: TreeString[];
}

/**
 * Complete mapping data structure
 */
export interface Complete48TreeMapping {
  /** The fundamental structure */
  structure: OnePlusFortyEightStructure;
  
  /** All 48 tree analyses */
  trees: TreeAnalysis[];
  
  /** Conceptual correspondences */
  correspondences: TreePatternCorrespondence[];
  
  /** Statistics by shape */
  statistics: TreeShapeStatistics[];
  
  /** Individual tree-to-pattern mappings */
  mappings: TreeToPatternMapping[];
}
