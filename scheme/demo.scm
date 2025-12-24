;;; demo.scm
;;;
;;; Demonstration of the Integral Semiotic Realism Pattern Archetype
;;; Shows practical usage examples

;; Load the pattern archetype module
;; (load "pattern-archetype.scm")

;; ============================================================================
;; Demo 1: Understanding Consciousness through the Archetype
;; ============================================================================

(define (demo-consciousness)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  DEMO 1: Consciousness as Integral Semiotic Pattern         ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  ;; Create a consciousness archetype
  (define consciousness-archetype
    (make-integral-archetype
     'subjective-experience    ; Firstness: The "what it's like" of consciousness
     'neural-correlates       ; Secondness: The physical brain states
     'cognitive-interpretation ; Thirdness: The conceptual understanding
     (make-dynamic-pattern
      'consciousness-emergence
      '(neural-phenomenal-bridge)
      'increasing-integration)))
  
  (display "Consciousness Archetype Created:")
  (newline)
  (display-archetype consciousness-archetype)
  (newline))

;; ============================================================================
;; Demo 2: Language as a Semiotic Cycle
;; ============================================================================

(define (demo-language)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  DEMO 2: Language as Semiotic Process                       ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  ;; Create language semiotic cycle
  (define language-cycle
    (make-semiotic-cycle
     'linguistic-sign       ; The word or symbol (Firstness)
     'referent             ; The thing referred to (Secondness)
     'meaning))            ; The understood significance (Thirdness)
  
  (display "Language Semiotic Cycle:")
  (newline)
  (display-component language-cycle)
  (newline)
  (newline)
  
  ;; Show the continuous signification process
  (define word-meaning-process
    (continuous-signification
     'word-tree           ; Sign: the word "tree"
     'concept-of-tree     ; Interpretant: our understanding
     'actual-tree))       ; Object: a real tree
  
  (display "Word-to-Meaning Process:")
  (newline)
  (display-component word-meaning-process)
  (newline))

;; ============================================================================
;; Demo 3: Scientific Knowledge Formation
;; ============================================================================

(define (demo-science)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  DEMO 3: Scientific Knowledge through Triadic Lens          ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  ;; Create scientific knowledge archetype
  (define science-archetype
    (make-integral-archetype
     'observation-data      ; Firstness: Raw empirical observations
     'natural-phenomena     ; Secondness: The actual physical world
     'theoretical-model     ; Thirdness: Scientific theories and laws
     (make-dynamic-pattern
      'scientific-method
      '(hypothesis-test-theory)
      'progressive-refinement)))
  
  (display "Scientific Knowledge Archetype:")
  (newline)
  (newline)
  
  ;; Show the processes
  (display "Epistemic Emergence - How observations arise:")
  (newline)
  (define obs-emergence
    (epistemic-emergence nondual-origin 'measurement-event))
  (display-component obs-emergence)
  (newline)
  (newline)
  
  (display "Ontological Emergence - How physical reality manifests:")
  (newline)
  (define phys-emergence
    (ontological-emergence nondual-origin 'quantum-event))
  (display-component phys-emergence)
  (newline))

;; ============================================================================
;; Demo 4: Perspectives and Their Integration
;; ============================================================================

(define (demo-perspectives)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  DEMO 4: Integration of Perspectives                        ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  (display "Perspectival Mapping:")
  (newline)
  (display "  1st Person (Subjective)  → Firstness  → Epistemology")
  (newline)
  (display "  2nd Person (Interpretive) → Thirdness → Methodology")
  (newline)
  (display "  3rd Person (Objective)    → Secondness → Ontology")
  (newline)
  (newline)
  
  (display "Complete Perspective Map:")
  (newline)
  (for-each
   (lambda (mapping)
     (display "  ")
     (display (car mapping))
     (display " → ")
     (display (cdr mapping))
     (newline))
   perspective-map)
  (newline))

;; ============================================================================
;; Demo 5: NonDual Origin and Evolution
;; ============================================================================

(define (demo-nondual)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  DEMO 5: NonDual Origin - Source of All Distinctions        ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  (display "The NonDual Origin:")
  (newline)
  (display-component nondual-origin)
  (newline)
  (newline)
  
  (display "NonDual Evolution - The Complete Cycle:")
  (newline)
  (define evolution (nondual-evolution))
  (display-component evolution)
  (newline)
  (newline)
  
  (display "The Infinity Loop Dynamic:")
  (newline)
  (display "  Left Cycle:  NonDual Origin → Epistemic Emergence → Firstness → NonDual Return")
  (newline)
  (display "  Right Cycle: NonDual Origin → Ontological Emergence → Secondness → NonDual Return")
  (newline)
  (display "  Upper Arc:   Firstness → Continuous Signification → Thirdness → Secondness")
  (newline)
  (display "  Lower Arc:   Secondness → Continuous Signification → Enactment → Firstness")
  (newline)
  (newline))

;; ============================================================================
;; Demo 6: Practical Application - Understanding Perception
;; ============================================================================

(define (demo-perception)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  DEMO 6: Perception as Integral Semiotic Process            ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  ;; Model visual perception
  (display "Visual Perception of an Apple:")
  (newline)
  (newline)
  
  ;; Firstness - the immediate sensory quality
  (define visual-quale (make-firstness 'red-round-sensation 'immediate-visual-impression))
  (display "Firstness - Immediate Visual Quality:")
  (newline)
  (display-component visual-quale)
  (newline)
  (newline)
  
  ;; Secondness - the actual apple
  (define actual-apple (make-secondness 'physical-apple 'object-in-world))
  (display "Secondness - The Actual Apple:")
  (newline)
  (display-component actual-apple)
  (newline)
  (newline)
  
  ;; Thirdness - the concept/recognition
  (define apple-concept (make-thirdness 'apple-recognition 'fruit-category))
  (display "Thirdness - The Concept 'Apple':")
  (newline)
  (display-component apple-concept)
  (newline)
  (newline)
  
  ;; The signification process
  (define perception-process
    (continuous-signification
     'visual-sensation
     'conceptual-recognition
     'external-object))
  (display "Continuous Signification (Perception Process):")
  (newline)
  (display-component perception-process)
  (newline))

;; ============================================================================
;; Demo 7: Complex Perspectival Systems - Meta-types
;; ============================================================================

(define (demo-complex-systems)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  DEMO 7: Complex Perspectival Systems                       ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  ;; Create a meta-type for self-referential systems
  (define self-ref-metatype
    (make-meta-type
     'self-referential-consciousness
     '(observer-observed-unity strange-loop)))
  
  (display "Meta-type: Self-Referential Consciousness")
  (newline)
  (display-component self-ref-metatype)
  (newline)
  (newline)
  
  ;; Create a cosmic habit for evolutionary development
  (define evolution-habit
    (make-cosmic-habit
     'complexification-tendency
     'emergent-hierarchy))
  
  (display "Cosmic Habit: Evolutionary Complexification")
  (newline)
  (display-component evolution-habit)
  (newline)
  (newline)
  
  ;; Combine into dynamic pattern
  (define consciousness-pattern
    (make-dynamic-pattern
     'integrated-consciousness
     '(neural-phenomenal-integration)
     'self-organizing-emergence))
  
  (display "Dynamic Pattern: Integrated Consciousness")
  (newline)
  (display-component consciousness-pattern)
  (newline))

;; ============================================================================
;; Run All Demonstrations
;; ============================================================================

(define (run-all-demos)
  (display "")
  (newline)
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║                                                               ║")
  (newline)
  (display "║  INTEGRAL SEMIOTIC REALISM PATTERN ARCHETYPE DEMONSTRATIONS  ║")
  (newline)
  (display "║                                                               ║")
  (newline)
  (display "║  A Pure Scheme Implementation of the Pattern Dynamics        ║")
  (newline)
  (display "║  Triadic Archetype Based on Peircean Semiotics              ║")
  (newline)
  (display "║                                                               ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline)
  
  (demo-consciousness)
  (newline)
  (newline)
  
  (demo-language)
  (newline)
  (newline)
  
  (demo-science)
  (newline)
  (newline)
  
  (demo-perspectives)
  (newline)
  (newline)
  
  (demo-nondual)
  (newline)
  (newline)
  
  (demo-perception)
  (newline)
  (newline)
  
  (demo-complex-systems)
  (newline)
  (newline)
  
  (display "╔═══════════════════════════════════════════════════════════════╗")
  (newline)
  (display "║  End of Demonstrations                                       ║")
  (newline)
  (display "╚═══════════════════════════════════════════════════════════════╝")
  (newline)
  (newline))

;; To run all demonstrations, uncomment:
;; (run-all-demos)

;; To run individual demos:
;; (demo-consciousness)
;; (demo-language)
;; (demo-science)
;; (demo-perspectives)
;; (demo-nondual)
;; (demo-perception)
;; (demo-complex-systems)
;;; Demonstration of the Pattern Primary Defining Archetype
;;; This script shows the Integral Semiotic Realism model in action

(load "pattern-primary-archetype.scm")

;; Display the complete framework
(display-framework)

(newline)
(display "================================================================================")
(newline)
(newline)

;; Run the demonstration
(demonstrate-framework)

;; Additional explorations
(display "=== ADDITIONAL EXPLORATIONS ===") (newline) (newline)

;; Query specific nodes
(display "7. QUERYING FIRSTNESS PROPERTIES:") (newline)
(display "   Aspect: ") (display (node-property firstness 'aspect)) (newline)
(display "   Quality: ") (display (node-property firstness 'quality)) (newline)
(display "   Mode: ") (display (node-property firstness 'mode)) (newline)
(display "   Character: ") (display (node-property firstness 'character)) (newline)
(newline)

;; Show perspective descriptions
(display "8. PERSPECTIVE SYSTEM:") (newline)
(display "   1st Person (") (display (perspective-label first-person-perspective))
(display "): ") (display (perspective-description first-person-perspective)) (newline)
(display "   2nd Person (") (display (perspective-label second-person-perspective))
(display "): ") (display (perspective-description second-person-perspective)) (newline)
(display "   3rd Person (") (display (perspective-label third-person-perspective))
(display "): ") (display (perspective-description third-person-perspective)) (newline)
(newline)

;; Demonstrate cosmic habit formation
(display "9. COSMIC HABIT FORMATION:") (newline)
(let ((habit (make-cosmic-habit 
              'meaning-making-pattern
              'triadic-semiosis
              'high
              1000)))
  (display "   ") (display habit) (newline))
(newline)

;; Show complete pattern archetype
(display "10. COMPLETE PATTERN PRIMARY ARCHETYPE:") (newline)
(display "    ") (display pattern-primary-archetype) (newline)
(newline)

(display "=== END OF DEMONSTRATION ===") (newline)
