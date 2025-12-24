;;; test-pattern.scm
;;;
;;; Test suite for the Integral Semiotic Realism Pattern Archetype
;;; Demonstrates the usage and validates the implementation

;; Load the pattern archetype module
;; (load "pattern-archetype.scm")

;; ============================================================================
;; Test Helper Functions
;; ============================================================================

(define (test-name name)
  (display "TEST: ")
  (display name)
  (newline))

(define (assert-equal actual expected message)
  (if (equal? actual expected)
      (begin
        (display "  ✓ PASS: ")
        (display message)
        (newline))
      (begin
        (display "  ✗ FAIL: ")
        (display message)
        (newline)
        (display "    Expected: ")
        (display expected)
        (newline)
        (display "    Actual: ")
        (display actual)
        (newline))))

(define (assert-true condition message)
  (if condition
      (begin
        (display "  ✓ PASS: ")
        (display message)
        (newline))
      (begin
        (display "  ✗ FAIL: ")
        (display message)
        (newline))))

;; ============================================================================
;; Test Suite 1: Basic Category Creation
;; ============================================================================

(define (test-basic-categories)
  (test-name "Basic Category Creation")
  
  ;; Test Firstness creation
  (define first (make-firstness 'red-quale 'pure-redness))
  (assert-equal (car first) 'firstness "Firstness tag is correct")
  (assert-equal (cdr (assq 'perspective (cdr first))) '1st-person "Firstness has 1st person perspective")
  (assert-equal (cdr (assq 'domain (cdr first))) 'epistemology "Firstness is epistemic")
  
  ;; Test Secondness creation
  (define second (make-secondness 'physical-apple 'actual-thing))
  (assert-equal (car second) 'secondness "Secondness tag is correct")
  (assert-equal (cdr (assq 'perspective (cdr second))) '3rd-person "Secondness has 3rd person perspective")
  (assert-equal (cdr (assq 'domain (cdr second))) 'ontology "Secondness is ontological")
  
  ;; Test Thirdness creation
  (define third (make-thirdness 'apple-concept 'fruit-law))
  (assert-equal (car third) 'thirdness "Thirdness tag is correct")
  (assert-equal (cdr (assq 'perspective (cdr third))) '2nd-person "Thirdness has 2nd person perspective")
  (assert-equal (cdr (assq 'domain (cdr third))) 'methodology "Thirdness is methodological")
  
  (newline))

;; ============================================================================
;; Test Suite 2: Semiotic Processes
;; ============================================================================

(define (test-semiotic-processes)
  (test-name "Semiotic Processes")
  
  ;; Test continuous signification
  (define cont-sign (continuous-signification 'sense-data 'concept 'real-object))
  (assert-equal (car cont-sign) 'continuous-signification "Process is continuous signification")
  (assert-equal (cdr (assq 'process (cdr cont-sign))) 'method "It's a methodological process")
  (assert-equal (cdr (assq 'from (cdr cont-sign))) 'firstness "Starts from firstness")
  (assert-equal (cdr (assq 'to (cdr cont-sign))) 'secondness "Ends at secondness")
  
  ;; Test epistemic emergence
  (define epist-emerg (epistemic-emergence nondual-origin 'awareness))
  (assert-equal (car epist-emerg) 'epistemic-emergence "Process is epistemic emergence")
  (assert-equal (cdr (assq 'from (cdr epist-emerg))) 'nondual-origin "Emerges from nondual origin")
  (assert-equal (cdr (assq 'to (cdr epist-emerg))) 'firstness "Emerges to firstness")
  
  ;; Test ontological emergence
  (define ontol-emerg (ontological-emergence nondual-origin 'being))
  (assert-equal (car ontol-emerg) 'ontological-emergence "Process is ontological emergence")
  (assert-equal (cdr (assq 'direction (cdr ontol-emerg))) 'objective "Has objective direction")
  
  (newline))

;; ============================================================================
;; Test Suite 3: Domains and Zones
;; ============================================================================

(define (test-domains-and-zones)
  (test-name "Domains and Zones")
  
  ;; Test actual domain
  (assert-equal (car actual-domain) 'actual-domain "Actual domain exists")
  (assert-equal (cdr (assq 'nature (cdr actual-domain))) 'manifest "Actual domain is manifest")
  
  ;; Test intransitive domain
  (assert-equal (car intransitive-domain) 'intransitive-domain "Intransitive domain exists")
  (assert-equal (cdr (assq 'zone (cdr intransitive-domain))) 'subsistence "Intransitive is zone of subsistence")
  
  ;; Test empirical domain
  (assert-equal (car empirical-domain) 'empirical-domain "Empirical domain exists")
  (assert-equal (cdr (assq 'zone (cdr empirical-domain))) 'existence "Empirical is zone of existence")
  
  ;; Test nondual origin
  (assert-equal (car nondual-origin) 'nondual-origin "NonDual origin exists")
  (assert-equal (cdr (assq 'zone (cdr nondual-origin))) 'subsistence "NonDual origin is zone of subsistence")
  
  (newline))

;; ============================================================================
;; Test Suite 4: Complete Semiotic Cycle
;; ============================================================================

(define (test-semiotic-cycle)
  (test-name "Complete Semiotic Cycle")
  
  (define cycle (make-semiotic-cycle 
                 'perception 
                 'world 
                 'understanding))
  
  (assert-equal (car cycle) 'semiotic-cycle "Created semiotic cycle")
  
  ;; Check nodes
  (define nodes (cdr (assq 'nodes (cdr cycle))))
  (assert-equal (length nodes) 3 "Cycle has 3 nodes")
  (assert-equal (car (car nodes)) 'firstness "First node is firstness")
  (assert-equal (car (cadr nodes)) 'secondness "Second node is secondness")
  (assert-equal (car (caddr nodes)) 'thirdness "Third node is thirdness")
  
  ;; Check processes
  (define processes (cdr (assq 'processes (cdr cycle))))
  (assert-true (> (length processes) 3) "Cycle has multiple processes")
  
  ;; Check origin
  (define origin (cdr (assq 'origin (cdr cycle))))
  (assert-equal (car origin) 'nondual-origin "Cycle references nondual origin")
  
  (newline))

;; ============================================================================
;; Test Suite 5: Perspectival System
;; ============================================================================

(define (test-perspectival-system)
  (test-name "Perspectival System")
  
  ;; Test perspective mapping
  (define first-person-cat (perspective->category '1st-person))
  (assert-equal (car first-person-cat) 'firstness "1st person maps to firstness")
  
  (define second-person-cat (perspective->category '2nd-person))
  (assert-equal (car second-person-cat) 'thirdness "2nd person maps to thirdness")
  
  (define third-person-cat (perspective->category '3rd-person))
  (assert-equal (car third-person-cat) 'secondness "3rd person maps to secondness")
  
  (newline))

;; ============================================================================
;; Test Suite 6: Meta-types and Cosmic Habits
;; ============================================================================

(define (test-meta-patterns)
  (test-name "Meta-types and Cosmic Habits")
  
  ;; Test meta-type
  (define meta (make-meta-type 'consciousness-metatype '(integrated-information)))
  (assert-equal (car meta) 'meta-type "Created meta-type")
  (assert-equal (cdr (assq 'level (cdr meta))) 'transcendent "Meta-type is transcendent")
  
  ;; Test cosmic habit
  (define habit (make-cosmic-habit 'evolution-habit 'increasing-complexity))
  (assert-equal (car habit) 'cosmic-habit "Created cosmic habit")
  (assert-equal (cdr (assq 'temporality (cdr habit))) 'evolutionary "Cosmic habit is evolutionary")
  
  ;; Test dynamic pattern
  (define pattern (make-dynamic-pattern 
                   'mind-pattern
                   '(mental-structure)
                   'self-organization))
  (assert-equal (car pattern) 'dynamic-pattern "Created dynamic pattern")
  
  (newline))

;; ============================================================================
;; Test Suite 7: Integral Archetype
;; ============================================================================

(define (test-integral-archetype)
  (test-name "Integral Archetype")
  
  (define meta-pat (make-dynamic-pattern
                    'test-pattern
                    '(test-structure)
                    'test-evolution))
  
  (define archetype (make-integral-archetype
                     'sign-test
                     'object-test
                     'interpretant-test
                     meta-pat))
  
  (assert-equal (car archetype) 'integral-archetype "Created integral archetype")
  
  ;; Check components
  (assert-true (assq 'semiotic-cycle (cdr archetype)) "Has semiotic cycle")
  (assert-true (assq 'meta-pattern (cdr archetype)) "Has meta pattern")
  (assert-true (assq 'nondual-origin (cdr archetype)) "Has nondual origin")
  (assert-true (assq 'perspectival-system (cdr archetype)) "Has perspectival system")
  
  (newline))

;; ============================================================================
;; Test Suite 8: Example Archetype
;; ============================================================================

(define (test-example-archetype)
  (test-name "Example Archetype Instance")
  
  (assert-equal (car example-archetype) 'integral-archetype "Example is an integral archetype")
  
  ;; Extract and verify components
  (define cycle (cdr (assq 'semiotic-cycle (cdr example-archetype))))
  (assert-equal (car cycle) 'semiotic-cycle "Example has semiotic cycle")
  
  (define nodes (cdr (assq 'nodes (cdr cycle))))
  (assert-equal (length nodes) 3 "Example cycle has 3 nodes")
  
  (newline))

;; ============================================================================
;; Run All Tests
;; ============================================================================

(define (run-all-tests)
  (display "")
  (newline)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  INTEGRAL SEMIOTIC REALISM PATTERN ARCHETYPE - TEST SUITE   ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  (test-basic-categories)
  (test-semiotic-processes)
  (test-domains-and-zones)
  (test-semiotic-cycle)
  (test-perspectival-system)
  (test-meta-patterns)
  (test-integral-archetype)
  (test-example-archetype)
  
  (display "")
  (newline)
  (display "═══════════════════════════════════════════════════════════════")
  (newline)
  (display "All tests completed!")
  (newline)
  (newline))

;; Run tests when this file is executed
;; Uncomment the following line to run tests
;; (run-all-tests)
