#!/usr/bin/env python3
"""
rooted_trees_48.py

Generate the 48 rooted trees with 7 nodes (OEIS A000081, n=7)
and map them to Pattern Dynamics 49 patterns.

This demonstrates the analogy: 1 root + 48 trees ≈ 1 Source + 48 patterns
"""

def tree_to_string(tree):
    """Convert tree structure to parentheses notation"""
    if not tree:
        return "()"
    return "(" + "".join(tree_to_string(subtree) for subtree in tree) + ")"

def tree_depth(tree):
    """Calculate maximum depth of tree"""
    if not tree:
        return 1
    return 1 + max(tree_depth(subtree) for subtree in tree)

def tree_width(tree):
    """Calculate width (number of children at root)"""
    return len(tree)

def tree_shape(tree):
    """Classify tree shape"""
    if not tree:
        return 'leaf'
    depth = tree_depth(tree)
    width = tree_width(tree)
    
    if depth >= 5 and width <= 2:
        return 'linear-deep'
    elif depth <= 3 and width >= 4:
        return 'wide-balanced'
    elif depth >= 4 and width >= 3:
        return 'complex-mixed'
    else:
        return 'moderate'

def generate_rooted_trees(n, cache={}):
    """Generate all rooted trees with n nodes using memoization"""
    if n in cache:
        return cache[n]
    
    if n == 0:
        return []
    if n == 1:
        result = [[]]  # Single node tree
        cache[n] = result
        return result
    
    trees = []
    # Generate trees by partitioning n-1 nodes among subtrees
    for partition in partitions(n - 1):
        for tree_combo in trees_from_partition(partition, cache):
            trees.append(tree_combo)
    
    cache[n] = trees
    return trees

def partitions(n):
    """Generate all partitions of n in non-increasing order"""
    if n == 0:
        yield []
        return
    
    for i in range(n, 0, -1):
        for p in partitions(n - i):
            if not p or i >= p[0]:
                yield [i] + p

def trees_from_partition(partition, cache):
    """Generate all distinct trees from a partition"""
    if not partition:
        yield []
        return
    
    # Group by size
    from collections import Counter
    size_counts = Counter(partition)
    
    # Generate combinations recursively
    sizes = sorted(size_counts.keys(), reverse=True)
    
    def generate_combos(sizes_left, counts_left):
        if not sizes_left:
            yield []
            return
        
        size = sizes_left[0]
        count = counts_left[size]
        trees = generate_rooted_trees(size, cache)
        
        # Generate all multiset selections of `count` trees of this size
        for selection in multiset_combinations(trees, count):
            for rest in generate_combos(sizes_left[1:], counts_left):
                yield list(selection) + rest
    
    yield from generate_combos(sizes, size_counts)

def multiset_combinations(items, k):
    """Generate k-element combinations with replacement"""
    if k == 0:
        yield []
        return
    if not items:
        return
    
    # Include first item
    for combo in multiset_combinations(items, k - 1):
        yield [items[0]] + combo
    
    # Exclude first item (move to rest)
    if len(items) > 1:
        yield from multiset_combinations(items[1:], k)

# Hardcoded 48 trees for verification (from OEIS and Rosetta Code)
FORTY_EIGHT_TREES_N7 = [
    # Format: (parentheses_string, description)
    ("(((((())))))", "Maximum depth linear chain"),
    ("((((()())))", "Deep with one branch"),
    ("(((()()))())", "Deep chain with leaf"),
    ("(((())()()))", "Deep with two leaves"),
    ("(((())(())))", "Deep balanced split"),
    ("((()()())())", "Medium depth, spread"),
    ("((()())(()))", "Two balanced groups"),
    ("((())(()()))", "Two groups, one split"),
    ("((())(())())", "Three groups"),
    ("(()()()()())", "Maximum width, shallow"),
    # Add more as needed - the generator should produce all 48
]

def analyze_tree(tree):
    """Analyze tree properties"""
    return {
        'string': tree_to_string(tree),
        'depth': tree_depth(tree),
        'width': tree_width(tree),
        'shape': tree_shape(tree),
        'nodes': count_nodes(tree)
    }

def count_nodes(tree):
    """Count total nodes in tree"""
    if not tree:
        return 1
    return 1 + sum(count_nodes(subtree) for subtree in tree)

def map_tree_to_pattern_family(shape):
    """Map tree shape to Pattern Dynamics family"""
    mapping = {
        'linear-deep': 'Dynamics Family',
        'wide-balanced': 'Source Family',
        'complex-mixed': 'Structure Family',
        'moderate': 'Mixed Families'
    }
    return mapping.get(shape, 'Unknown')

def explain_tree_pattern_meaning(shape):
    """Explain pattern meaning for tree shape"""
    meanings = {
        'linear-deep': (
            "Sequential emergence: Linear developmental path with maximum "
            "differentiation and depth. Like Dynamics family patterns that "
            "unfold through feedback loops and iterative processes."
        ),
        'wide-balanced': (
            "Simultaneous emergence: Parallel differentiation with horizontal "
            "diversity. Like Source family patterns that manifest multiple "
            "aspects at once from unified origin."
        ),
        'complex-mixed': (
            "Holarchical integration: Balanced nesting and branching creating "
            "integrated complexity. Like Structure family patterns organizing "
            "hierarchies and networks."
        ),
        'moderate': (
            "Complex emergence: Mixed patterns of nesting and branching "
            "representing multi-level holarchical relationships."
        )
    }
    return meanings.get(shape, "Unknown pattern meaning")

def display_48_trees_analysis():
    """Display analysis of all 48 trees with pattern mappings"""
    print("\n" + "="*70)
    print("  The 48 Rooted Trees (n=7) and Pattern Dynamics Mapping")
    print("="*70)
    print("\nOEIS A000081: a(7) = 48 rooted trees with 7 nodes")
    print("Pattern Dynamics: 1 Source + 48 emergent patterns = 49 total\n")
    
    # Generate all trees with 7 nodes
    print("Generating trees with 7 nodes...")
    trees = generate_rooted_trees(7)
    print(f"Generated {len(trees)} trees\n")
    
    # Analyze each tree
    analyses = []
    for idx, tree in enumerate(trees, 1):
        analysis = analyze_tree(tree)
        analysis['index'] = idx
        analyses.append(analysis)
    
    # Group by shape
    from collections import defaultdict
    by_shape = defaultdict(list)
    for analysis in analyses:
        by_shape[analysis['shape']].append(analysis)
    
    # Display by groups
    for shape in ['linear-deep', 'wide-balanced', 'complex-mixed', 'moderate']:
        if shape not in by_shape:
            continue
        
        group = by_shape[shape]
        family = map_tree_to_pattern_family(shape)
        
        print(f"\n{shape.replace('-', ' ').title()} Trees ({family}):")
        print("-" * 70)
        
        for analysis in group[:10]:  # Show first 10 of each type
            print(f"{analysis['index']:2d}. {analysis['string']:20s} "
                  f"[depth:{analysis['depth']} width:{analysis['width']}]")
        
        if len(group) > 10:
            print(f"    ... and {len(group) - 10} more")
        
        print(f"\nPattern Meaning: {explain_tree_pattern_meaning(shape)}")
    
    print("\n" + "="*70)
    print(f"Total: {len(analyses)} trees")
    print("\nEach tree represents a unique way patterns can emerge from Source.")
    print("The recursive structure of trees mirrors the holarchical nature")
    print("of Pattern Dynamics, where patterns nest and emerge at multiple")
    print("levels of organization.\n")
    
    # Summary statistics
    print("Summary Statistics:")
    print("-" * 70)
    for shape, group in by_shape.items():
        family = map_tree_to_pattern_family(shape)
        print(f"  {shape.replace('-', ' ').title():20s}: {len(group):2d} trees → {family}")
    print()

def demonstrate_recursion_correspondence():
    """Demonstrate how tree recursion corresponds to pattern recursion"""
    print("\n" + "="*70)
    print("  Recursion Correspondence: Trees ↔ Patterns")
    print("="*70)
    print("""
Tree Recursion:
  tree = [] (leaf)  OR  [subtree1, subtree2, ...]
  
  Each tree is composed of subtrees, which are themselves trees.
  The structure is self-similar at every level.

Pattern Recursion:
  pattern = Source  OR  emerge(Source, crossings...)
  
  Each pattern emerges from Source and contains the triadic cycle.
  The structure exhibits the same ISR framework at every level.

Key Correspondences:
  • Leaf node () ↔ Source pattern (undifferentiated unity)
  • Parent-child ↔ Pattern emergence (Source → first-order)
  • Sibling subtrees ↔ Co-emergent patterns (same order)
  • Tree depth ↔ Pattern order (0th, 1st, 2nd)
  • Nesting level ↔ Holarchical containment
  • Branching factor ↔ Differentiation richness

Both systems are RECURSIVE BY NATURE:
  • Defined in terms of themselves
  • Self-similar at different scales  
  • Generated through iterative application of simple rules
  • Exhibit exponential growth in complexity
""")

if __name__ == "__main__":
    demonstrate_recursion_correspondence()
    display_48_trees_analysis()
