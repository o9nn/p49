;;; pattern-archetype.scm
;;; 
;;; Implementation of the Integral Semiotic Realism Pattern Primary Defining Archetype
;;; Based on the triadic relationship of Sign, Object, and Interpretant
;;; with NonDual Origin as the zone of subsistence

;; ============================================================================
;; Core Semiotic Categories (Peirce's Triadic Categories)
;; ============================================================================

;;; Firstness: Quality, possibility, feeling, immediate presence
;;; The Sign aspect - epistemic, subjective, 1st person perspective
;;; 
;;; @param sign - The sign or representamen (symbol, quality, or feeling)
;;; @param quality - The qualitative character or immediate impression
;;; @return An association list representing a Firstness category
(define (make-firstness sign quality)
  `(firstness
    (sign . ,sign)
    (quality . ,quality)
    (perspective . 1st-person)
    (domain . epistemology)
    (category . possibility)))

;;; Secondness: Existence, actuality, brute fact, resistance
;;; The Object aspect - ontological, objective, 3rd person perspective
;;;
;;; @param object - The object or referent (thing, fact, or brute existence)
;;; @param actuality - The actual existence or concrete particularity
;;; @return An association list representing a Secondness category
(define (make-secondness object actuality)
  `(secondness
    (object . ,object)
    (actuality . ,actuality)
    (perspective . 3rd-person)
    (domain . ontology)
    (category . existence)))

;;; Thirdness: Mediation, law, habit, continuity
;;; The Interpretant aspect - methodological, intersubjective, 2nd person perspective
;;;
;;; @param interpretant - The interpretant or meaning (concept, law, or habit)
;;; @param law - The general law, rule, or pattern of interpretation
;;; @return An association list representing a Thirdness category
(define (make-thirdness interpretant law)
  `(thirdness
    (interpretant . ,interpretant)
    (law . ,law)
    (perspective . 2nd-person)
    (domain . methodology)
    (category . mediation)))

;; ============================================================================
;; NonDual Origin - The Zone of Subsistence
;; ============================================================================

;; The intransitive domain where all perspectival distinctions arise
(define nondual-origin
  `(nondual-origin
    (zone . subsistence)
    (domain . intransitive)
    (nature . undifferentiated)
    (potentiality . infinite)
    (description . "The zone from which all semiotic processes emerge and to which they return")))

;; ============================================================================
;; Domains and Zones
;; ============================================================================

;; Actual Domain: Manifest experience and concrete phenomena
(define actual-domain
  `(actual-domain
    (nature . manifest)
    (contents . (epistemic-signs ontological-objects))
    (description . "Domain of actualized forms where signs and objects exist")))

;; Intransitive Domain: Zone of Subsistence - Pure relationality
(define intransitive-domain
  `(intransitive-domain
    (nature . relational)
    (zone . subsistence)
    (contents . (semiotic-processes meaning-making))
    (description . "Realm of pure relationality before actualization")))

;; Empirical Domain: Zone of Existence - Lived experience
(define empirical-domain
  `(empirical-domain
    (nature . experiential)
    (zone . existence)
    (contents . (enactment practice))
    (description . "Realm of lived experience and empirical manifestation")))

;; ============================================================================
;; Semiotic Processes - The Cyclical Dynamics
;; ============================================================================

;; Continuous Signification (Method): Thirdness mediating between Firstness and Secondness
(define (continuous-signification sign interpretant object)
  `(continuous-signification
    (process . method)
    (from . firstness)
    (through . thirdness)
    (to . secondness)
    (sign . ,sign)
    (interpretant . ,interpretant)
    (object . ,object)
    (description . "The ongoing process of sign interpretation and meaning-making")))

;; Epistemic Emergence: Movement from NonDual Origin to Firstness/Sign
(define (epistemic-emergence origin sign)
  `(epistemic-emergence
    (from . nondual-origin)
    (to . firstness)
    (origin . ,origin)
    (emergent . ,sign)
    (direction . subjective)
    (description . "The emergence of epistemic awareness from undifferentiated origin")))

;; Ontological Emergence: Movement from NonDual Origin to Secondness/Object
(define (ontological-emergence origin object)
  `(ontological-emergence
    (from . nondual-origin)
    (to . secondness)
    (origin . ,origin)
    (emergent . ,object)
    (direction . objective)
    (description . "The emergence of ontological reality from undifferentiated origin")))

;; NonDual Return: Movement back to origin through evolution
(define (nondual-return entity)
  `(nondual-return
    (from . ,(car entity))
    (to . nondual-origin)
    (entity . ,entity)
    (process . evolution)
    (description . "The dynamic return to undifferentiated origin")))

;; NonDual Evolution: The complete cycle of emergence and return
(define (nondual-evolution)
  `(nondual-evolution
    (left-cycle . (epistemic-emergence firstness nondual-return))
    (right-cycle . (ontological-emergence secondness nondual-return))
    (description . "The dynamic cycle of emergence from and return to NonDual Origin")))

;; ============================================================================
;; The Complete Semiotic Cycle
;; ============================================================================

;; Create a complete semiotic cycle
(define (make-semiotic-cycle sign object interpretant)
  (let ((firstness-node (make-firstness sign 'immediate-quality))
        (secondness-node (make-secondness object 'brute-existence))
        (thirdness-node (make-thirdness interpretant 'mediating-law)))
    `(semiotic-cycle
      (nodes . (,firstness-node ,secondness-node ,thirdness-node))
      (processes . 
        (,(continuous-signification sign interpretant object)
         ,(epistemic-emergence nondual-origin sign)
         ,(ontological-emergence nondual-origin object)
         ,(nondual-return firstness-node)
         ,(nondual-return secondness-node)))
      (origin . ,nondual-origin)
      (evolution . ,(nondual-evolution))
      (domains . (,actual-domain ,intransitive-domain ,empirical-domain)))))

;; ============================================================================
;; Perspectival System
;; ============================================================================

;; Map perspectives to their corresponding categories
(define perspective-map
  `((1st-person . (firstness subject epistemology))
    (2nd-person . (thirdness interpretant methodology))
    (3rd-person . (secondness object ontology))))

;; Get category for a perspective
(define (perspective->category perspective)
  (cdr (assq perspective perspective-map)))

;; ============================================================================
;; Complex Perspectival Systems - Meta-types and Cosmic Habits
;; ============================================================================

;; Meta-type: A pattern that transcends individual instances
(define (make-meta-type name pattern-structure)
  `(meta-type
    (name . ,name)
    (structure . ,pattern-structure)
    (level . transcendent)
    (description . "A pattern structure that organizes multiple perspectives")))

;; Cosmic Habit: A dynamic pattern that evolves over time
(define (make-cosmic-habit name evolutionary-tendency)
  `(cosmic-habit
    (name . ,name)
    (tendency . ,evolutionary-tendency)
    (temporality . evolutionary)
    (description . "A habit-like pattern that shapes cosmic evolution")))

;; Dynamic Pattern: Combines meta-type and cosmic habit
(define (make-dynamic-pattern name structure tendency)
  `(dynamic-pattern
    (meta-type . ,(make-meta-type name structure))
    (cosmic-habit . ,(make-cosmic-habit name tendency))
    (integration . complex-perspectival-system)))

;; ============================================================================
;; Integral Archetype - The Complete Pattern
;; ============================================================================

;; The Pattern Primary Defining Archetype integrates all components
(define (make-integral-archetype sign object interpretant meta-pattern)
  (let ((cycle (make-semiotic-cycle sign object interpretant)))
    `(integral-archetype
      (name . "Integral Semiotic Realism Pattern")
      (semiotic-cycle . ,cycle)
      (meta-pattern . ,meta-pattern)
      (nondual-origin . ,nondual-origin)
      (perspectival-system . ,perspective-map)
      (description . "The complete pattern integrating triadic semiotics with nondual origin"))))

;; ============================================================================
;; Query and Transformation Functions
;; ============================================================================

;; Extract a component from the archetype
(define (archetype-get archetype key)
  (cdr (assq key archetype)))

;; Get all nodes from a semiotic cycle
(define (get-cycle-nodes cycle)
  (cdr (assq 'nodes (cdr (assq 'semiotic-cycle cycle)))))

;; Get all processes from a semiotic cycle
(define (get-cycle-processes cycle)
  (cdr (assq 'processes (cdr (assq 'semiotic-cycle cycle)))))

;; Transform a node by applying a function to its components
(define (transform-node node f)
  (cons (car node)
        (map (lambda (pair) 
               (cons (car pair) (f (cdr pair))))
             (cdr node))))

;; ============================================================================
;; Example: Creating a Concrete Instance
;; ============================================================================

;; Create a concrete instance of the pattern archetype
(define example-archetype
  (make-integral-archetype
   'phenomenal-experience     ; Sign (firstness)
   'physical-reality         ; Object (secondness)
   'conceptual-understanding ; Interpretant (thirdness)
   (make-dynamic-pattern
    'consciousness-pattern
    '(subject-object-relation)
    'evolutionary-complexification)))

;; ============================================================================
;; Display Functions
;; ============================================================================

;; Pretty print an archetype component
(define (display-component component)
  (if (pair? component)
      (begin
        (display (car component))
        (newline)
        (for-each
         (lambda (item)
           (if (pair? item)
               (begin
                 (display "  ")
                 (display (car item))
                 (display ": ")
                 (display (cdr item))
                 (newline))))
         (cdr component)))
      (display component)))

;; Display the complete archetype
(define (display-archetype archetype)
  (display "=== INTEGRAL SEMIOTIC REALISM PATTERN ARCHETYPE ===")
  (newline)
  (newline)
  (for-each display-component (cdr archetype)))

;; ============================================================================
;; Export key definitions
;; ============================================================================

;; This allows the module to be used by other Scheme programs
(define exports
  `((make-firstness . ,make-firstness)
    (make-secondness . ,make-secondness)
    (make-thirdness . ,make-thirdness)
    (make-semiotic-cycle . ,make-semiotic-cycle)
    (make-integral-archetype . ,make-integral-archetype)
    (make-dynamic-pattern . ,make-dynamic-pattern)
    (nondual-origin . ,nondual-origin)
    (perspective-map . ,perspective-map)
    (example-archetype . ,example-archetype)
    (display-archetype . ,display-archetype)))
