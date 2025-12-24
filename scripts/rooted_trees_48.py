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

def tree_symmetry_type(tree):
    """
    Analyze tree symmetry based on @drzo's insight:
    - bilateral: symmetric structure like ((A)(A))
    - asymmetric-paired: different elements in containers like ((A)(B))
    - unique: totally asymmetric structure (U)
    - other: mixed or complex symmetry
    """
    if not tree:
        return 'unique'  # Single node is unique
    
    if len(tree) == 0:
        return 'unique'
    
    if len(tree) == 1:
        # Single child - check if it has symmetry
        return tree_symmetry_type(tree[0])
    
    # Check for bilateral symmetry (all children identical)
    if len(tree) >= 2:
        first_str = tree_to_string(tree[0])
        if all(tree_to_string(child) == first_str for child in tree):
            return 'bilateral-symmetric'
    
    # Check for paired asymmetry (2 children, different)
    if len(tree) == 2:
        if tree_to_string(tree[0]) != tree_to_string(tree[1]):
            return 'asymmetric-paired'
    
    # Check for total uniqueness (all children different)
    child_strings = [tree_to_string(child) for child in tree]
    if len(child_strings) == len(set(child_strings)):
        return 'unique-asymmetric'
    
    return 'mixed-symmetry'

def symmetry_to_pattern_mapping(symmetry):
    """
    Map symmetry types to Pattern Dynamics patterns based on @drzo's insight:
    - Polarity: bilateral symmetry about centre ((A)(A))
    - Exchange: asymmetric elements in separate containers ((A)(B))
    - Creativity: total asymmetry of uniqueness (U)
    """
    mapping = {
        'bilateral-symmetric': 'Polarity',
        'asymmetric-paired': 'Exchange', 
        'unique-asymmetric': 'Creativity',
        'unique': 'Creativity',
        'mixed-symmetry': 'Mixed'
    }
    return mapping.get(symmetry, 'Other')

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
    """Analyze tree properties including symmetry"""
    symmetry = tree_symmetry_type(tree)
    return {
        'string': tree_to_string(tree),
        'depth': tree_depth(tree),
        'width': tree_width(tree),
        'shape': tree_shape(tree),
        'nodes': count_nodes(tree),
        'symmetry': symmetry,
        'symmetry_pattern': symmetry_to_pattern_mapping(symmetry)
    }

def classify_tree_to_family(tree, tree_idx):
    """
    Refined classification: Assign each tree to one of 7 families
    Each family gets exactly 7 trees (Source gets 6 + itself as root = 7)
    
    Uses multi-dimensional criteria:
    - Depth and width (structural)
    - Symmetry (relational)
    - Special patterns (unique characteristics)
    
    The 7 families:
    1. Source (6 trees) - wide, balanced, foundational
    2. Dynamics (7 trees) - linear deep, sequential
    3. Creativity (7 trees) - unique, asymmetric, novel
    4. Exchange (7 trees) - asymmetric pairs, reciprocal
    5. Structure (7 trees) - complex mixed, hierarchical
    6. Polarity (7 trees) - bilateral symmetric, dual
    7. Rhythm (7 trees) - moderate, rhythmic patterns
    """
    depth = tree_depth(tree)
    width = tree_width(tree)
    symmetry = tree_symmetry_type(tree)
    tree_str = tree_to_string(tree)
    
    # SOURCE FAMILY (6 trees): Widest, most balanced, foundational structures
    # Characteristics: width >= 4, shallow depth
    if width >= 5:
        return 'Source'
    if width == 4 and depth <= 3:
        return 'Source'
    
    # POLARITY FAMILY (7 trees): Perfect bilateral symmetry
    # Characteristics: bilateral-symmetric with clear mirror structure
    if symmetry == 'bilateral-symmetric':
        # Take 7 most clearly bilateral trees
        if width >= 2 or (width == 1 and depth >= 5):
            return 'Polarity'
    
    # CREATIVITY FAMILY (7 trees): Unique asymmetry, total differentiation
    # Characteristics: all children unique OR highly asymmetric
    if symmetry == 'unique-asymmetric':
        return 'Creativity'
    if symmetry == 'unique':
        return 'Creativity'
    # Also include some mixed-symmetry with high uniqueness
    if symmetry == 'mixed-symmetry' and width >= 3:
        return 'Creativity'
    
    # EXCHANGE FAMILY (7 trees): Asymmetric pairs, reciprocal structures
    # Characteristics: exactly 2 different children (asymmetric-paired)
    if symmetry == 'asymmetric-paired':
        # Select 7 most representative exchange patterns
        if depth >= 4 and depth <= 6:
            return 'Exchange'
    
    # STRUCTURE FAMILY (7 trees): Complex hierarchical, balanced complexity
    # Characteristics: depth 4-5, width 2-3, complex nesting
    if depth >= 4 and depth <= 5 and width >= 2 and width <= 3:
        if symmetry != 'bilateral-symmetric' and symmetry != 'asymmetric-paired':
            return 'Structure'
    
    # DYNAMICS FAMILY (7 trees): Linear deep, sequential emergence
    # Characteristics: maximum depth (6-7), narrow width (1-2)
    if depth >= 6 and width <= 2:
        return 'Dynamics'
    if depth == 5 and width == 1:
        return 'Dynamics'
    
    # RHYTHM FAMILY (7 trees): Moderate, rhythmic, remaining patterns
    # Characteristics: moderate depth/width, mixed characteristics
    return 'Rhythm'

def refine_family_assignments(trees):
    """
    Refine the family assignments to ensure exactly 7 trees per family
    (6 for Source + Source itself as root = 7 total, shown as 6)
    """
    analyses = []
    for idx, tree in enumerate(trees, 1):
        analysis = analyze_tree(tree)
        analysis['index'] = idx
        analysis['family'] = classify_tree_to_family(tree, idx)
        analyses.append(analysis)
    
    # Count trees per family
    from collections import Counter
    family_counts = Counter(a['family'] for a in analyses)
    
    # Target counts
    target_counts = {
        'Source': 6,
        'Dynamics': 7,
        'Creativity': 7,
        'Exchange': 7,
        'Structure': 7,
        'Polarity': 7,
        'Rhythm': 7
    }
    
    # Iterative balancing - multiple passes
    max_iterations = 10
    for iteration in range(max_iterations):
        family_counts = Counter(a['family'] for a in analyses)
        balanced = all(family_counts[f] == target_counts[f] for f in target_counts)
        
        if balanced:
            break
        
        # Find families that need adjustment
        needs_more = [(f, target_counts[f] - family_counts[f]) 
                      for f in target_counts if family_counts[f] < target_counts[f]]
        has_extra = [(f, family_counts[f] - target_counts[f]) 
                     for f in target_counts if family_counts[f] > target_counts[f]]
        
        # Transfer trees from families with extras to families that need more
        for from_family, extra_count in sorted(has_extra, key=lambda x: -x[1]):
            if not needs_more:
                break
                
            # Get trees in this family sorted by fit score (worst first)
            candidates = [a for a in analyses if a['family'] == from_family]
            candidates.sort(key=lambda a: score_tree_for_family(a, from_family))
            
            for candidate in candidates[:extra_count]:
                if not needs_more:
                    break
                
                # Find best target family for this tree
                best_family = None
                best_score = -1
                
                for to_family, needed in needs_more:
                    score = score_tree_for_family(candidate, to_family)
                    if score > best_score:
                        best_score = score
                        best_family = to_family
                
                if best_family:
                    candidate['family'] = best_family
                    # Update needs_more list
                    needs_more = [(f, target_counts[f] - (family_counts[f] + (1 if f == best_family else 0) - (1 if f == from_family else 0))) 
                                  for f in target_counts if f in dict(needs_more) or f == best_family]
                    needs_more = [(f, n) for f, n in needs_more if n > 0]
    
    return analyses

def score_tree_for_family(analysis, family):
    """Score how well a tree fits a given family (higher = better fit)"""
    score = 0
    depth = analysis['depth']
    width = analysis['width']
    symmetry = analysis['symmetry']
    
    if family == 'Source':
        # Source: Wide, balanced, foundational (prefer width >= 4)
        score += width * 3  # Strong preference for wider trees
        score += max(0, 5 - depth)  # Prefer shallower trees
        if width >= 5: score += 15
        if width == 4: score += 10
        if depth <= 3: score += 5
        
    elif family == 'Dynamics':
        # Dynamics: Deep, linear, sequential (prefer depth >= 6, width <= 2)
        score += depth * 3  # Strong preference for deeper trees
        score += max(0, 3 - width)  # Prefer narrower trees
        if depth >= 6: score += 15
        if depth == 5 and width == 1: score += 10
        if width == 1: score += 8
        
    elif family == 'Creativity':
        # Creativity: Unique, asymmetric, novel
        if symmetry in ['unique-asymmetric', 'unique']: score += 25
        if symmetry == 'mixed-symmetry': score += 10
        if width >= 3: score += 8  # Prefer multiple unique branches
        if depth >= 4: score += 5
        
    elif family == 'Exchange':
        # Exchange: Asymmetric pairs, reciprocal (prefer exactly 2 different children)
        if symmetry == 'asymmetric-paired': score += 25
        if width == 2: score += 10
        if 4 <= depth <= 5: score += 8
        if depth == 4: score += 5
        
    elif family == 'Structure':
        # Structure: Complex hierarchical (prefer depth 4-5, width 2-3)
        if 4 <= depth <= 5: score += 10
        if 2 <= width <= 3: score += 10
        if symmetry == 'mixed-symmetry': score += 8
        if depth == 4 and width == 2: score += 5
        
    elif family == 'Polarity':
        # Polarity: Bilateral symmetric, dual
        if symmetry == 'bilateral-symmetric': score += 25
        if width >= 2 and width <= 4: score += 8
        if width == 2: score += 5
        # Also include some moderate depth trees with symmetry
        if depth >= 4 and depth <= 6: score += 3
        
    elif family == 'Rhythm':
        # Rhythm: Moderate, rhythmic, catch-all (prefer moderate values)
        if 3 <= depth <= 5: score += 8
        if 1 <= width <= 3: score += 8
        if symmetry == 'mixed-symmetry': score += 5
        if depth == 4 and width == 1: score += 10
    
    return score

def find_best_alternative_family(analysis, target_counts, current_counts):
    """Find the best alternative family for a tree"""
    best_family = 'Rhythm'
    best_score = -1
    
    for family in target_counts.keys():
        if current_counts[family] < target_counts[family]:
            score = score_tree_for_family(analysis, family)
            if score > best_score:
                best_score = score
                best_family = family
    
    return best_family

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

def display_refined_7x7_classification():
    """
    Display the refined 7x7 classification where each of 7 families has exactly 7 trees
    (Source has 6 trees + itself as root = 7 total, appearing as 6 + 1 = 49 → 48)
    """
    print("\n" + "="*70)
    print("  REFINED 7×7 Classification: 48 Trees → 7 Families")
    print("="*70)
    print("\nPattern Dynamics Structure:")
    print("  Source (root) + 6 trees = 7 total → appears as 1 + 48 = 49")
    print("  6 other families × 7 trees each = 42 trees")
    print("  Total: 6 + 42 = 48 trees across 7 families\n")
    
    # Generate and refine
    trees = generate_rooted_trees(7)
    analyses = refine_family_assignments(trees)
    
    # Group by family
    from collections import defaultdict
    by_family = defaultdict(list)
    for analysis in analyses:
        by_family[analysis['family']].append(analysis)
    
    # Display each family
    family_order = ['Source', 'Dynamics', 'Creativity', 'Exchange', 'Structure', 'Polarity', 'Rhythm']
    
    for family in family_order:
        if family not in by_family:
            continue
            
        group = by_family[family]
        expected = 6 if family == 'Source' else 7
        
        print(f"\n{family} Family: {len(group)} trees (target: {expected})")
        print("-" * 70)
        
        if family == 'Source':
            print("  (Source itself is the root, these 6 trees represent its aspects)")
        
        for analysis in sorted(group, key=lambda a: a['index']):
            symmetry_label = analysis['symmetry_pattern']
            print(f"  {analysis['index']:2d}. {analysis['string']:22s} "
                  f"[d:{analysis['depth']} w:{analysis['width']} sym:{symmetry_label}]")
    
    print("\n" + "="*70)
    print("  Family Distribution Summary")
    print("="*70)
    
    for family in family_order:
        count = len(by_family[family])
        expected = 6 if family == 'Source' else 7
        status = "✓" if count == expected else f"✗ ({count} vs {expected})"
        print(f"  {family:12s}: {count:2d} trees {status}")
    
    total = sum(len(by_family[f]) for f in family_order)
    print(f"\n  TOTAL: {total} trees (48 expected)")
    
    print("\n" + "="*70)
    print("This represents the complete 7×7 holarchical structure:")
    print("  7 families × 7 patterns = 49 (with Source as both root and family)")
    print("  Manifests as: 1 Source root + 48 emergent patterns")
    print("="*70)

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

def display_symmetry_analysis():
    """
    Display symmetry-based analysis of trees mapping to specific patterns
    Based on @drzo's insight about symmetry types
    """
    print("\n" + "="*70)
    print("  Symmetry-Based Pattern Mapping (@drzo's Insight)")
    print("="*70)
    print("""
Tree symmetry reveals specific pattern correspondences:

1. POLARITY → Bilateral Symmetry: ((A)(A))
   Trees with identical children reflect the dual nature of polarity.
   Example: ((())(()))  - perfect mirror symmetry
   
2. EXCHANGE → Asymmetric Pairs: ((A)(B))
   Trees with two different children represent exchange between
   distinct elements in separate containers.
   Example: ((())())  - different subtrees, balanced exchange
   
3. CREATIVITY → Unique Asymmetry: (U)
   Trees where all children are unique represent total creativity
   and the emergence of novelty.
   Example: (()(())())  - each branch is distinct
""")
    
    # Generate trees and analyze symmetry
    trees = generate_rooted_trees(7)
    analyses = [analyze_tree(tree) for tree in trees]
    
    # Group by symmetry
    from collections import defaultdict
    by_symmetry = defaultdict(list)
    for idx, analysis in enumerate(analyses, 1):
        analysis['index'] = idx
        by_symmetry[analysis['symmetry']].append(analysis)
    
    print("\nSymmetry Distribution in the 48 Trees:")
    print("-" * 70)
    
    symmetry_labels = {
        'bilateral-symmetric': 'Bilateral Symmetric (Polarity)',
        'asymmetric-paired': 'Asymmetric Paired (Exchange)',
        'unique-asymmetric': 'Unique Asymmetric (Creativity)',
        'mixed-symmetry': 'Mixed Symmetry',
        'unique': 'Unique (Creativity)'
    }
    
    for sym_type, label in symmetry_labels.items():
        if sym_type in by_symmetry:
            group = by_symmetry[sym_type]
            print(f"\n{label}: {len(group)} trees")
            
            # Show examples
            for analysis in group[:5]:  # Show first 5 examples
                pattern = analysis['symmetry_pattern']
                print(f"  {analysis['index']:2d}. {analysis['string']:20s} → {pattern}")
            
            if len(group) > 5:
                print(f"      ... and {len(group) - 5} more")
    
    print("\n" + "="*70)
    print("This symmetry perspective adds another dimension to understanding")
    print("how tree structures correspond to specific Pattern Dynamics patterns.")
    print("="*70)
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
    display_refined_7x7_classification()
    # display_symmetry_analysis()  # Original symmetry analysis
    # display_48_trees_analysis()  # Original depth/width analysis
