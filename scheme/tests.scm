;;; Test Suite for Pattern Primary Archetype
;;; Validates the correctness of the implementation

(load "pattern-primary-archetype.scm")

;; ============================================================================
;; Test Framework
;; ============================================================================

(define test-count 0)
(define test-passed 0)
(define test-failed 0)

(define (test-equal description expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
        (set! test-passed (+ test-passed 1))
        (display "✓ PASS: ")
        (display description)
        (newline))
      (begin
        (set! test-failed (+ test-failed 1))
        (display "✗ FAIL: ")
        (display description)
        (newline)
        (display "  Expected: ")
        (display expected)
        (newline)
        (display "  Actual: ")
        (display actual)
        (newline))))

(define (test-true description predicate)
  (test-equal description #t predicate))

(define (test-false description predicate)
  (test-equal description #f predicate))

(define (display-test-summary)
  (newline)
  (display "=== TEST SUMMARY ===")
  (newline)
  (display "Total tests: ")
  (display test-count)
  (newline)
  (display "Passed: ")
  (display test-passed)
  (newline)
  (display "Failed: ")
  (display test-failed)
  (newline)
  (if (= test-failed 0)
      (begin
        (display "✓ ALL TESTS PASSED!")
        (newline))
      (begin
        (display "✗ SOME TESTS FAILED")
        (newline))))

;; ============================================================================
;; Data Structure Tests
;; ============================================================================

(define (test-data-structures)
  (display "=== TESTING DATA STRUCTURES ===")
  (newline)
  
  ;; Test perspective creation
  (test-true "Perspective is recognized"
             (perspective? first-person-perspective))
  
  (test-equal "Perspective type"
              'first-person
              (perspective-type first-person-perspective))
  
  (test-equal "Perspective label"
              "Subject"
              (perspective-label first-person-perspective))
  
  ;; Test domain creation
  (test-true "Domain is recognized"
             (domain? actual-domain))
  
  (test-equal "Domain name"
              'actual
              (domain-name actual-domain))
  
  (test-equal "Domain zone"
              'manifest
              (domain-zone actual-domain))
  
  ;; Test semiotic node creation
  (test-true "Semiotic node is recognized"
             (semiotic-node? firstness))
  
  (test-equal "Node name"
              'firstness
              (node-name firstness))
  
  (test-equal "Node category"
              'sign
              (node-category firstness))
  
  ;; Test process creation
  (test-true "Process is recognized"
             (process? continuous-signification))
  
  (test-equal "Process name"
              'continuous-signification
              (process-name continuous-signification))
  
  (newline))

;; ============================================================================
;; Triadic Structure Tests
;; ============================================================================

(define (test-triadic-structure)
  (display "=== TESTING TRIADIC STRUCTURE ===")
  (newline)
  
  ;; Test that we have three main nodes
  (test-equal "Semiotic triad has 3 nodes"
              3
              (length semiotic-triad))
  
  ;; Test firstness properties
  (test-equal "Firstness aspect"
              'epistemology
              (node-property firstness 'aspect))
  
  (test-equal "Firstness quality"
              'immediacy
              (node-property firstness 'quality))
  
  (test-equal "Firstness mode"
              'possibility
              (node-property firstness 'mode))
  
  ;; Test secondness properties
  (test-equal "Secondness aspect"
              'ontology
              (node-property secondness 'aspect))
  
  (test-equal "Secondness quality"
              'reaction
              (node-property secondness 'quality))
  
  ;; Test thirdness properties
  (test-equal "Thirdness aspect"
              'methodology
              (node-property thirdness 'aspect))
  
  (test-equal "Thirdness quality"
              'mediation
              (node-property thirdness 'quality))
  
  (newline))

;; ============================================================================
;; Process Tests
;; ============================================================================

(define (test-processes)
  (display "=== TESTING PROCESSES ===")
  (newline)
  
  ;; Test that we have 4 fundamental processes
  (test-equal "Semiotic cycle has 4 processes"
              4
              (length semiotic-cycle))
  
  ;; Test continuous signification
  (test-equal "Continuous signification source"
              'thirdness
              (node-name (process-source continuous-signification)))
  
  (test-equal "Continuous signification target"
              'firstness
              (node-name (process-target continuous-signification)))
  
  (test-equal "Continuous signification method"
              'methodological
              (process-method continuous-signification))
  
  ;; Test epistemic emergence
  (test-equal "Epistemic emergence source"
              'firstness
              (node-name (process-source epistemic-emergence)))
  
  (test-equal "Epistemic emergence target"
              'thirdness
              (node-name (process-target epistemic-emergence)))
  
  ;; Test ontological emergence
  (test-equal "Ontological emergence source"
              'secondness
              (node-name (process-source ontological-emergence)))
  
  (test-equal "Ontological emergence target"
              'thirdness
              (node-name (process-target ontological-emergence)))
  
  ;; Test nondual return
  (test-equal "NonDual return source"
              'enactment
              (node-name (process-source nondual-return)))
  
  (test-equal "NonDual return target"
              'nondual-origin
              (node-name (process-target nondual-return)))
  
  (newline))

;; ============================================================================
;; Domain Tests
;; ============================================================================

(define (test-domains)
  (display "=== TESTING DOMAINS ===")
  (newline)
  
  ;; Test that we have 3 domains
  (test-equal "Total domains"
              3
              (length all-domains))
  
  ;; Test firstness and secondness are in actual domain
  (test-equal "Firstness domain"
              'actual
              (domain-name (node-domain firstness)))
  
  (test-equal "Secondness domain"
              'actual
              (domain-name (node-domain secondness)))
  
  ;; Test thirdness and nondual-origin are in intransitive domain
  (test-equal "Thirdness domain"
              'intransitive
              (domain-name (node-domain thirdness)))
  
  (test-equal "NonDual origin domain"
              'intransitive
              (domain-name (node-domain nondual-origin)))
  
  ;; Test enactment is in empirical domain
  (test-equal "Enactment domain"
              'empirical
              (domain-name (node-domain enactment)))
  
  ;; Test domain zones
  (test-equal "Actual domain zone"
              'manifest
              (domain-zone actual-domain))
  
  (test-equal "Intransitive domain zone"
              'subsistence
              (domain-zone intransitive-domain))
  
  (test-equal "Empirical domain zone"
              'existence
              (domain-zone empirical-domain))
  
  (newline))

;; ============================================================================
;; Perspective Tests
;; ============================================================================

(define (test-perspectives)
  (display "=== TESTING PERSPECTIVES ===")
  (newline)
  
  ;; Test that we have 5 perspectives
  (test-equal "Total perspectives"
              5
              (length all-perspectives))
  
  ;; Test node perspectives
  (test-equal "Firstness perspective type"
              'first-person
              (perspective-type (node-perspective firstness)))
  
  (test-equal "Secondness perspective type"
              'third-person
              (perspective-type (node-perspective secondness)))
  
  (test-equal "Thirdness perspective type"
              'second-person
              (perspective-type (node-perspective thirdness)))
  
  (test-equal "NonDual origin perspective type"
              'pre-perspectival
              (perspective-type (node-perspective nondual-origin)))
  
  (test-equal "Enactment perspective type"
              'multi-perspectival
              (perspective-type (node-perspective enactment)))
  
  (newline))

;; ============================================================================
;; Query Function Tests
;; ============================================================================

(define (test-query-functions)
  (display "=== TESTING QUERY FUNCTIONS ===")
  (newline)
  
  ;; Test find-node
  (test-equal "Find node by name"
              'firstness
              (node-name (find-node 'firstness all-nodes)))
  
  (test-equal "Find non-existent node"
              #f
              (find-node 'nonexistent all-nodes))
  
  ;; Test processes-from-node
  (let ((procs (processes-from-node firstness semiotic-cycle)))
    (test-equal "Processes from firstness count"
                1
                (length procs))
    (test-equal "Process from firstness is epistemic-emergence"
                'epistemic-emergence
                (process-name (car procs))))
  
  ;; Test processes-to-node
  (let ((procs (processes-to-node thirdness semiotic-cycle)))
    (test-equal "Processes to thirdness count"
                2
                (length procs)))
  
  (newline))

;; ============================================================================
;; Cycle Traversal Tests
;; ============================================================================

(define (test-cycle-traversal)
  (display "=== TESTING CYCLE TRAVERSAL ===")
  (newline)
  
  ;; Test traverse-cycle from firstness
  (let ((path (traverse-cycle firstness 3)))
    (test-equal "Traverse 3 steps from firstness"
                '(firstness thirdness firstness thirdness)
                (map node-name path)))
  
  ;; Test traverse-cycle from secondness
  (let ((path (traverse-cycle secondness 2)))
    (test-equal "Traverse 2 steps from secondness"
                '(secondness thirdness firstness)
                (map node-name path)))
  
  (newline))

;; ============================================================================
;; NonDual Tests
;; ============================================================================

(define (test-nondual)
  (display "=== TESTING NONDUAL ELEMENTS ===")
  (newline)
  
  ;; Test nondual-origin properties
  (test-equal "NonDual origin aspect"
              'unity
              (node-property nondual-origin 'aspect))
  
  (test-equal "NonDual origin quality"
              'undifferentiated
              (node-property nondual-origin 'quality))
  
  ;; Test nondual-evolution
  (let ((evolution (nondual-evolution nondual-origin)))
    (test-true "NonDual evolution is a list"
               (list? evolution))
    (test-equal "NonDual evolution tag"
                'evolution
                (car evolution)))
  
  ;; Test nondual-return-cycle
  (let ((return-cycle (nondual-return-cycle all-nodes)))
    (test-true "NonDual return cycle is a list"
               (list? return-cycle))
    (test-equal "NonDual return cycle tag"
                'return
                (car return-cycle)))
  
  (newline))

;; ============================================================================
;; Pattern Dynamics Tests
;; ============================================================================

(define (test-pattern-dynamics)
  (display "=== TESTING PATTERN DYNAMICS ===")
  (newline)
  
  ;; Test cosmic habit creation
  (let ((habit (make-cosmic-habit 'test-habit 'triadic 'high 100)))
    (test-true "Cosmic habit is a list"
               (list? habit))
    (test-equal "Cosmic habit tag"
                'cosmic-habit
                (car habit)))
  
  ;; Test meta-type creation
  (let ((meta (make-meta-type 'test-meta all-perspectives all-domains 'high)))
    (test-true "Meta-type is a list"
               (list? meta))
    (test-equal "Meta-type tag"
                'meta-type
                (car meta)))
  
  ;; Test pattern evolution
  (let ((evolved (evolve-pattern 'initial 2)))
    (test-true "Evolved pattern is a list"
               (list? evolved)))
  
  (newline))

;; ============================================================================
;; Integration Tests
;; ============================================================================

(define (test-integration)
  (display "=== TESTING INTEGRATION ===")
  (newline)
  
  ;; Test that all nodes are accessible
  (test-equal "All nodes count"
              5
              (length all-nodes))
  
  ;; Test that pattern-primary-archetype is properly formed
  (test-true "Pattern primary archetype is a list"
             (list? pattern-primary-archetype))
  
  (test-equal "Pattern primary archetype tag"
              'pattern-primary-archetype
              (car pattern-primary-archetype))
  
  ;; Test transformation
  (let ((transformed (apply-semiotic-transformation 'test-data continuous-signification)))
    (test-true "Transformation produces a list"
               (list? transformed))
    (test-equal "Transformation tag"
                'transformed-data
                (car transformed)))
  
  (newline))

;; ============================================================================
;; Run All Tests
;; ============================================================================

(define (run-all-tests)
  (display "=== RUNNING ALL TESTS FOR PATTERN PRIMARY ARCHETYPE ===")
  (newline)
  (newline)
  
  (test-data-structures)
  (test-triadic-structure)
  (test-processes)
  (test-domains)
  (test-perspectives)
  (test-query-functions)
  (test-cycle-traversal)
  (test-nondual)
  (test-pattern-dynamics)
  (test-integration)
  
  (display-test-summary)
  (newline))

;; Run tests when loaded
(run-all-tests)
