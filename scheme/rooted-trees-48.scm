#!/usr/bin/env guile
!#

;;; rooted-trees-48.scm
;;; Generate the 48 rooted trees with 7 nodes (OEIS A000081, n=7)
;;; and map them to Pattern Dynamics 49 patterns
;;;
;;; This implements the analogy: 1 root + 48 trees ≈ 1 Source + 48 patterns

;;; ============================================================================
;;; ROOTED TREE GENERATION (OEIS A000081)
;;; ============================================================================

;;; A rooted tree is represented as a list:
;;; - '() represents a leaf (single node)
;;; - (tree1 tree2 ...) represents a root with subtrees

(define (tree->string tree)
  "Convert a tree to parentheses notation"
  (if (null? tree)
      "()"
      (string-append "(" 
                     (apply string-append 
                            (map tree->string tree))
                     ")")))

;;; Generate all rooted trees with n nodes
;;; Using the recursive algorithm from OEIS A000081
(define (rooted-trees n)
  "Generate all unlabeled rooted trees with n nodes"
  (cond 
    ((= n 0) '())
    ((= n 1) '(()))  ; Single node tree
    (else
     ;; Generate trees by partitioning n-1 nodes among subtrees
     (apply append
            (map (lambda (partition)
                   (trees-from-partition partition))
                 (partitions (- n 1)))))))

;;; Generate integer partitions
(define (partitions n)
  "Generate all partitions of n in non-increasing order"
  (if (<= n 0)
      '(())
      (let loop ((n n) (max-val n))
        (if (= n 0)
            '(())
            (apply append
                   (map (lambda (k)
                          (map (lambda (p) (cons k p))
                               (loop (- n k) k)))
                        (filter (lambda (k) (<= k (min n max-val)))
                                (iota n 1))))))))

;;; Generate trees from a partition
(define (trees-from-partition partition)
  "Generate all distinct trees from a partition of node counts"
  (if (null? partition)
      '(())
      ;; For each element in partition, get all trees of that size
      ;; Then combine them in all possible ways
      (let* ((sizes (remove-duplicates partition))
             (size-groups (map (lambda (s)
                                (cons s (count (lambda (x) (= x s)) partition)))
                              sizes)))
        (combine-tree-groups size-groups))))

;;; Combine groups of trees
(define (combine-tree-groups groups)
  "Combine tree groups respecting multiplicity"
  (if (null? groups)
      '(())
      (let* ((group (car groups))
             (size (car group))
             (count (cdr group))
             (trees (rooted-trees size))
             (rest-combinations (combine-tree-groups (cdr groups))))
        (apply append
               (map (lambda (rest)
                      (map (lambda (selection)
                             (append selection rest))
                           (multiset-combinations trees count)))
                    rest-combinations)))))

;;; Generate multiset combinations (combinations with replacement)
(define (multiset-combinations items k)
  "Generate all ways to select k items from list (with replacement)"
  (if (= k 0)
      '(())
      (if (null? items)
          '()
          (append
           (map (lambda (combo) (cons (car items) combo))
                (multiset-combinations items (- k 1)))
           (multiset-combinations (cdr items) k)))))

;;; Utility: count occurrences
(define (count pred lst)
  (length (filter pred lst)))

;;; Utility: remove duplicates
(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (x) (not (equal? x (car lst)))) 
                                      (cdr lst))))))

;;; ============================================================================
;;; CACHED GENERATION FOR EFFICIENCY
;;; ============================================================================

(define *tree-cache* (make-hash-table))

(define (rooted-trees-cached n)
  "Cached version of rooted-trees for efficiency"
  (or (hash-ref *tree-cache* n)
      (let ((trees (rooted-trees n)))
        (hash-set! *tree-cache* n trees)
        trees)))

;;; ============================================================================
;;; TREE PROPERTIES AND CLASSIFICATION
;;; ============================================================================

(define (tree-depth tree)
  "Calculate the maximum depth of a tree"
  (if (null? tree)
      1
      (+ 1 (apply max (map tree-depth tree)))))

(define (tree-width tree)
  "Calculate the width (number of children at root)"
  (length tree))

(define (tree-node-count tree)
  "Count total nodes in tree"
  (if (null? tree)
      1
      (+ 1 (apply + (map tree-node-count tree)))))

(define (tree-shape tree)
  "Classify tree shape: 'linear, 'balanced, or 'mixed"
  (cond
    ((null? tree) 'leaf)
    ((= (tree-width tree) 1) 
     (if (null? (car tree)) 'linear 'linear))
    ((> (tree-width tree) 2) 'balanced)
    (else 'mixed)))

;;; ============================================================================
;;; MAPPING TREES TO PATTERN DYNAMICS PATTERNS
;;; ============================================================================

;;; Load the Pattern Dynamics 49 implementation
(load "pattern-dynamics-49.scm")

;;; Map tree properties to pattern characteristics
(define (tree-pattern-correspondence tree)
  "Determine which pattern a tree structure corresponds to"
  (let ((depth (tree-depth tree))
        (width (tree-width tree))
        (shape (tree-shape tree)))
    `((tree . ,(tree->string tree))
      (depth . ,depth)
      (width . ,width)
      (shape . ,shape)
      (pattern-analogue . ,(infer-pattern-from-tree tree)))))

;;; Infer pattern meaning from tree structure
(define (infer-pattern-from-tree tree)
  "Map tree structure to Pattern Dynamics meaning"
  (let ((depth (tree-depth tree))
        (width (tree-width tree)))
    (cond
      ;; Linear chain (maximum depth) → Sequential emergence
      ((and (>= depth 5) (= width 1))
       '(type . sequential-emergence)
       '(meaning . "Linear developmental path, maximum differentiation")
       '(pattern-family . dynamics-family))
      
      ;; Wide branching (maximum width) → Simultaneous emergence
      ((>= width 4)
       '(type . simultaneous-emergence)
       '(meaning . "Parallel differentiation, horizontal diversity")
       '(pattern-family . source-family))
      
      ;; Balanced structure → Holarchical integration
      ((and (>= depth 3) (>= width 2))
       '(type . holarchical-integration)
       '(meaning . "Balanced nesting and branching, integrated complexity")
       '(pattern-family . structure-family))
      
      ;; Mixed structure → Complex emergence
      (else
       '(type . complex-emergence)
       '(meaning . "Mixed nesting and branching patterns")
       '(pattern-family . mixed-families)))))

;;; ============================================================================
;;; GENERATE AND MAP ALL 48 TREES
;;; ============================================================================

(define (generate-48-trees-with-mappings)
  "Generate all 48 rooted trees with 7 nodes and their pattern mappings"
  (let ((trees (rooted-trees-cached 7)))
    (display "Generating 48 rooted trees with 7 nodes...\n")
    (display (format #f "Found ~a trees\n\n" (length trees)))
    
    ;; Map each tree to its pattern correspondence
    (map (lambda (tree idx)
           `((index . ,idx)
             (tree . ,tree)
             (parentheses . ,(tree->string tree))
             (properties . ,(tree-pattern-correspondence tree))))
         trees
         (iota (length trees) 1))))

;;; ============================================================================
;;; THE 48+1 STRUCTURE: MAPPING TO PATTERN DYNAMICS
;;; ============================================================================

(define (explain-48-plus-1-structure)
  "Explain the 1+48 structure in both systems"
  (display "
╔═══════════════════════════════════════════════════════════════════╗
║  OEIS A000081 and Pattern Dynamics 49: The 1+48 Structure        ║
╚═══════════════════════════════════════════════════════════════════╝

OEIS A000081 (n=7):                    Pattern Dynamics 49:
─────────────────                      ─────────────────────

  1 Root Node                            1 Source Pattern
      |                                      |
      ↓                                      ↓
  48 Rooted Trees                        48 Emergent Patterns
  (7 nodes each)                         (6 + 42)

TOTAL: 1 + 48 = 49                     TOTAL: 1 + 6 + 42 = 49

The Structure of 7:
───────────────────
• 7 nodes in each tree            • 7 major aspects (Source + 6)
• 1 root + 6 others               • 1 Source + 6 first-order
• 48 ways to arrange              • 48 emergent patterns
• 7 × 7 = 49 total                • 7 × 7 matrix representation

Key Correspondences:
────────────────────
Root node        ⟷  Source pattern (NonDual Origin)
Child nodes      ⟷  Emergent patterns
Tree depth       ⟷  Pattern order (0th, 1st, 2nd)
Branching        ⟷  Holarchical crossing
Nesting          ⟷  Pattern containment
Parentheses ()   ⟷  Pattern boundaries
Recursion        ⟷  Self-similar structure
Tree structure   ⟷  Holarchical organization

"))
  (newline))

;;; ============================================================================
;;; DEMONSTRATION AND OUTPUT
;;; ============================================================================

(define (display-tree-pattern-mapping tree-data)
  "Display a formatted tree-pattern mapping"
  (let ((idx (cdr (assoc 'index tree-data)))
        (parens (cdr (assoc 'parentheses tree-data)))
        (props (cdr (assoc 'properties tree-data))))
    (display (format #f "~2d. ~a\n" idx parens))
    (display (format #f "    Depth: ~a, Width: ~a, Shape: ~a\n"
                     (cdr (assoc 'depth props))
                     (cdr (assoc 'width props))
                     (cdr (assoc 'shape props))))
    (newline)))

(define (demo-48-trees)
  "Demonstrate the generation and mapping of 48 trees"
  (explain-48-plus-1-structure)
  
  (display "Generating the 48 rooted trees with 7 nodes:\n")
  (display "═══════════════════════════════════════════════\n\n")
  
  (let ((mappings (generate-48-trees-with-mappings)))
    (for-each display-tree-pattern-mapping mappings)
    
    (display "\n")
    (display (format #f "Total: ~a trees generated\n" (length mappings)))
    (display "This corresponds to the 48 emergent patterns in Pattern Dynamics.\n")
    (display "Together with Source, we have 1 + 48 = 49 total patterns.\n\n")))

;;; ============================================================================
;;; EXPORT AND MAIN
;;; ============================================================================

(define (main args)
  "Main entry point"
  (demo-48-trees))

;; Run demo if executed directly
(when (and (defined? 'command-line)
           (string-suffix? "rooted-trees-48.scm" 
                          (car (command-line))))
  (main (command-line)))
