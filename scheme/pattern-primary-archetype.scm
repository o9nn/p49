;;; Pattern Primary Defining Archetype
;;; Implementation of Integral Semiotic Realism in Pure Scheme
;;;
;;; This module implements the triadic semiotic cycle shown in the
;;; Integral Semiotic Realism diagram, capturing the dynamic relationships
;;; between Firstness, Secondness, and Thirdness across multiple domains.

;; ============================================================================
;; Core Data Structures
;; ============================================================================

;;; Perspective - represents the three fundamental perspectives
(define (make-perspective type label description)
  (list 'perspective type label description))

(define (perspective? obj)
  (and (pair? obj) (eq? (car obj) 'perspective)))

(define (perspective-type p) (cadr p))
(define (perspective-label p) (caddr p))
(define (perspective-description p) (cadddr p))

;;; Domain - represents the ontological/epistemological domains
(define (make-domain name zone characteristics)
  (list 'domain name zone characteristics))

(define (domain? obj)
  (and (pair? obj) (eq? (car obj) 'domain)))

(define (domain-name d) (cadr d))
(define (domain-zone d) (caddr d))
(define (domain-characteristics d) (cadddr d))

;;; Semiotic Node - represents Firstness, Secondness, or Thirdness
(define (make-semiotic-node name category perspective domain properties)
  (list 'semiotic-node name category perspective domain properties))

(define (semiotic-node? obj)
  (and (pair? obj) (eq? (car obj) 'semiotic-node)))

(define (node-name n) (cadr n))
(define (node-category n) (caddr n))
(define (node-perspective n) (cadddr n))
(define (node-domain n) (car (cddddr n)))
(define (node-properties n) (cadr (cddddr n)))

;;; Process - represents the transformative relationships between nodes
(define (make-process name source target method characteristics)
  (list 'process name source target method characteristics))

(define (process? obj)
  (and (pair? obj) (eq? (car obj) 'process)))

(define (process-name p) (cadr p))
(define (process-source p) (caddr p))
(define (process-target p) (cadddr p))
(define (process-method p) (car (cddddr p)))
(define (process-characteristics p) (cadr (cddddr p)))

;; ============================================================================
;; Perspectives (1st, 2nd, 3rd Person)
;; ============================================================================

(define first-person-perspective
  (make-perspective 
   'first-person
   "Subject"
   "Epistemological perspective of the knower, the experiencing subject"))

(define second-person-perspective
  (make-perspective
   'second-person
   "Interpretant/Methodology"
   "Methodological perspective of interpretation and mediation"))

(define third-person-perspective
  (make-perspective
   'third-person
   "Object"
   "Ontological perspective of the known, the objective reality"))

(define pre-perspectival
  (make-perspective
   'pre-perspectival
   "NonDual Origin"
   "Pre-perspectival source from which all perspectives emerge"))

(define multi-perspectival
  (make-perspective
   'multi-perspectival
   "Integrated Perspectives"
   "Integration of all perspectives in lived experience"))

;; ============================================================================
;; Domains and Zones
;; ============================================================================

(define actual-domain
  (make-domain
   'actual
   'manifest
   '(phenomena experience actualized-forms concrete-reality)))

(define intransitive-domain
  (make-domain
   'intransitive
   'subsistence
   '(pure-relationality meaning-making nondual-origin potentiality)))

(define empirical-domain
  (make-domain
   'empirical
   'existence
   '(enactment lived-experience practice manifestation)))

;; ============================================================================
;; The Triadic Semiotic Structure
;; ============================================================================

;;; Firstness - The Sign (Epistemological, 1st Person)
(define firstness
  (make-semiotic-node
   'firstness
   'sign
   first-person-perspective
   actual-domain
   '((aspect . epistemology)
     (quality . immediacy)
     (mode . possibility)
     (character . feeling-quality))))

;;; Secondness - The Object (Ontological, 3rd Person)
(define secondness
  (make-semiotic-node
   'secondness
   'object
   third-person-perspective
   actual-domain
   '((aspect . ontology)
     (quality . reaction)
     (mode . actuality)
     (character . brute-fact))))

;;; Thirdness - The Interpretant (Methodological, 2nd Person)
(define thirdness
  (make-semiotic-node
   'thirdness
   'interpretant
   second-person-perspective
   intransitive-domain
   '((aspect . methodology)
     (quality . mediation)
     (mode . necessity)
     (character . law-habit))))

;;; NonDual Origin - The Zone of Subsistence
(define nondual-origin
  (make-semiotic-node
   'nondual-origin
   'source
   pre-perspectival
   intransitive-domain
   '((aspect . unity)
     (quality . undifferentiated)
     (mode . potential)
     (character . primordial))))

;;; Enactment - The Empirical Domain Process
(define enactment
  (make-semiotic-node
   'enactment
   'process
   multi-perspectival
   empirical-domain
   '((aspect . praxis)
     (quality . experiential)
     (mode . dynamic)
     (character . lived-reality))))

;; ============================================================================
;; The Four Fundamental Processes
;; ============================================================================

;;; Continuous Signification - Method from Thirdness to Firstness
(define continuous-signification
  (make-process
   'continuous-signification
   thirdness
   firstness
   'methodological
   '((direction . downward-left)
     (domain . actual)
     (nature . generative)
     (effect . sign-production))))

;;; Epistemic Emergence - From Firstness to Thirdness
(define epistemic-emergence
  (make-process
   'epistemic-emergence
   firstness
   thirdness
   'epistemological
   '((direction . upward-left)
     (domain . actual-to-intransitive)
     (nature . ascending)
     (effect . meaning-emergence))))

;;; Ontological Emergence - From Secondness to Thirdness
(define ontological-emergence
  (make-process
   'ontological-emergence
   secondness
   thirdness
   'ontological
   '((direction . upward-right)
     (domain . actual-to-intransitive)
     (nature . ascending)
     (effect . reality-emergence))))

;;; NonDual Return - From Empirical back through the cycle
(define nondual-return
  (make-process
   'nondual-return
   enactment
   nondual-origin
   'integrative
   '((direction . cyclical)
     (domain . empirical-to-intransitive)
     (nature . recursive)
     (effect . renewal))))

;; ============================================================================
;; The Complete Semiotic Cycle
;; ============================================================================

(define semiotic-cycle
  (list continuous-signification
        epistemic-emergence
        ontological-emergence
        nondual-return))

(define semiotic-triad
  (list firstness secondness thirdness))

(define all-nodes
  (list firstness secondness thirdness nondual-origin enactment))

(define all-domains
  (list actual-domain intransitive-domain empirical-domain))

(define all-perspectives
  (list first-person-perspective second-person-perspective third-person-perspective
        pre-perspectival multi-perspectival))

;; ============================================================================
;; Query and Navigation Functions
;; ============================================================================

;;; Get node by name
(define (find-node name nodes)
  (cond
    ((null? nodes) #f)
    ((eq? (node-name (car nodes)) name) (car nodes))
    (else (find-node name (cdr nodes)))))

;;; Get all processes from a given node
(define (processes-from-node node processes)
  (filter (lambda (p) 
            (equal? (process-source p) node))
          processes))

;;; Get all processes to a given node
(define (processes-to-node node processes)
  (filter (lambda (p)
            (equal? (process-target p) node))
          processes))

;;; Get property from node
(define (node-property node key)
  (let ((props (node-properties node)))
    (cond
      ((null? props) #f)
      ((eq? (caar props) key) (cdar props))
      (else (node-property 
             (make-semiotic-node 
              (node-name node)
              (node-category node)
              (node-perspective node)
              (node-domain node)
              (cdr props))
             key)))))

;; ============================================================================
;; Cycle Traversal and Transformation
;; ============================================================================

;;; Traverse the semiotic cycle starting from a node
(define (traverse-cycle start-node steps)
  (define (traverse-helper current remaining-steps path)
    (if (= remaining-steps 0)
        (reverse path)
        (let* ((outgoing (processes-from-node current semiotic-cycle))
               (next-process (if (null? outgoing) #f (car outgoing)))
               (next-node (if next-process 
                            (process-target next-process)
                            #f)))
          (if next-node
              (traverse-helper next-node 
                             (- remaining-steps 1)
                             (cons next-node path))
              (reverse path)))))
  (traverse-helper start-node steps (list start-node)))

;;; Apply a transformation through the cycle
(define (apply-semiotic-transformation data process)
  (let ((method (process-method process))
        (characteristics (process-characteristics process)))
    (list 'transformed-data
          data
          'via-process (process-name process)
          'using-method method
          'with-characteristics characteristics)))

;; ============================================================================
;; Evolution and Return Functions
;; ============================================================================

;;; NonDual Evolution - the outward movement from origin
(define (nondual-evolution origin)
  (list 'evolution
        'from origin
        'manifesting-as (list firstness secondness thirdness)
        'through 'differentiation))

;;; NonDual Return - the inward movement back to origin  
(define (nondual-return-cycle nodes)
  (list 'return
        'from nodes
        'returning-to nondual-origin
        'through 'integration))

;; ============================================================================
;; Display and Inspection Functions
;; ============================================================================

;;; Display node information
(define (display-node node)
  (display "Node: ") (display (node-name node)) (newline)
  (display "  Category: ") (display (node-category node)) (newline)
  (display "  Perspective: ") (display (perspective-label (node-perspective node))) (newline)
  (display "  Domain: ") (display (domain-name (node-domain node))) (newline)
  (display "  Properties: ") (display (node-properties node)) (newline))

;;; Display process information
(define (display-process proc)
  (display "Process: ") (display (process-name proc)) (newline)
  (display "  From: ") (display (node-name (process-source proc))) (newline)
  (display "  To: ") (display (node-name (process-target proc))) (newline)
  (display "  Method: ") (display (process-method proc)) (newline)
  (display "  Characteristics: ") (display (process-characteristics proc)) (newline))

;;; Display the entire framework
(define (display-framework)
  (display "=== INTEGRAL SEMIOTIC REALISM FRAMEWORK ===") (newline) (newline)
  
  (display "--- PERSPECTIVES ---") (newline)
  (for-each (lambda (p)
              (display (perspective-label p))
              (display ": ")
              (display (perspective-description p))
              (newline))
            all-perspectives)
  (newline)
  
  (display "--- DOMAINS ---") (newline)
  (for-each (lambda (d)
              (display (domain-name d))
              (display " (")
              (display (domain-zone d))
              (display "): ")
              (display (domain-characteristics d))
              (newline))
            all-domains)
  (newline)
  
  (display "--- SEMIOTIC NODES ---") (newline)
  (for-each display-node all-nodes)
  (newline)
  
  (display "--- SEMIOTIC PROCESSES ---") (newline)
  (for-each display-process semiotic-cycle)
  (newline))

;; ============================================================================
;; Pattern Dynamics - Cosmic Habits and Meta-types
;; ============================================================================

;;; A cosmic habit is a meta-pattern that emerges through semiotic cycles
(define (make-cosmic-habit name pattern-type stability iterations)
  (list 'cosmic-habit name pattern-type stability iterations))

;;; Dynamic pattern that evolves through the cycle
(define (evolve-pattern pattern cycles)
  (if (= cycles 0)
      pattern
      (evolve-pattern
       (list 'evolved-pattern
             'from pattern
             'through-cycle (traverse-cycle firstness 3))
       (- cycles 1))))

;;; Meta-type: A complex perspectival system
(define (make-meta-type name perspectives domains coherence)
  (list 'meta-type name
        'integrates perspectives
        'across domains
        'with-coherence coherence))

;; ============================================================================
;; Example Usage and Demonstration
;; ============================================================================

(define (demonstrate-framework)
  (display "=== DEMONSTRATION OF PATTERN PRIMARY ARCHETYPE ===") (newline) (newline)
  
  ;; Show the triadic structure
  (display "1. THE TRIADIC STRUCTURE:") (newline)
  (display "   Firstness (Sign/Epistemology/1st Person) <-> ") (newline)
  (display "   Secondness (Object/Ontology/3rd Person) <-> ") (newline)
  (display "   Thirdness (Interpretant/Methodology/2nd Person)") (newline)
  (newline)
  
  ;; Demonstrate cycle traversal
  (display "2. CYCLE TRAVERSAL from Firstness (3 steps):") (newline)
  (display "   ") (display (map node-name (traverse-cycle firstness 3))) (newline)
  (newline)
  
  ;; Show evolution from NonDual Origin
  (display "3. NONDUAL EVOLUTION:") (newline)
  (display "   ") (display (nondual-evolution nondual-origin)) (newline)
  (newline)
  
  ;; Demonstrate transformation
  (display "4. SEMIOTIC TRANSFORMATION:") (newline)
  (let ((data 'raw-experience))
    (display "   Input: ") (display data) (newline)
    (display "   After continuous-signification: ")
    (display (apply-semiotic-transformation data continuous-signification))
    (newline))
  (newline)
  
  ;; Show pattern evolution
  (display "5. PATTERN EVOLUTION (2 cycles):") (newline)
  (display "   ") (display (evolve-pattern 'initial-pattern 2)) (newline)
  (newline)
  
  ;; Create a meta-type
  (display "6. META-TYPE FORMATION:") (newline)
  (let ((mt (make-meta-type 
             'integral-semiotic-realism
             all-perspectives
             all-domains
             'high)))
    (display "   ") (display mt) (newline))
  (newline))

;; Export main definitions for external use
(define pattern-primary-archetype
  (list 'pattern-primary-archetype
        (list 'nodes all-nodes)
        (list 'processes semiotic-cycle)
        (list 'domains all-domains)
        (list 'perspectives all-perspectives)
        (list 'origin nondual-origin)))

;; Accessor functions for pattern-primary-archetype
(define (archetype-nodes archetype)
  (cadr (assoc 'nodes (cdr archetype))))

(define (archetype-processes archetype)
  (cadr (assoc 'processes (cdr archetype))))

(define (archetype-domains archetype)
  (cadr (assoc 'domains (cdr archetype))))

(define (archetype-perspectives archetype)
  (cadr (assoc 'perspectives (cdr archetype))))

(define (archetype-origin archetype)
  (cadr (assoc 'origin (cdr archetype))))
