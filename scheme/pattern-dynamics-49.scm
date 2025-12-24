#!/usr/bin/env guile
!#

;;; pattern-dynamics-49.scm
;;; Complete implementation of the 49 Pattern Dynamics patterns
;;; using Integral Semiotic Realism framework
;;;
;;; Structure:
;;; - 1 Zeroth-order pattern: Source (with 7 aspects)
;;; - 6 First-order patterns: Rhythm, Polarity, Structure, Exchange, Creativity, Dynamics
;;; - 42 Second-order patterns: 7 major aspects × 6 minor aspects = 42 patterns
;;; Total: 1 + 6 + 42 = 49 patterns

(load "pattern-archetype.scm")

;;; ============================================================================
;;; ZEROTH-ORDER PATTERN: SOURCE (Pattern 0)
;;; The foundational ground from which all patterns emerge
;;; ============================================================================

;;; Source is the NonDual Origin itself - the ground of all being
;;; It manifests through 7 aspects that correspond to the 7 first-order domains

(define (make-source-pattern)
  (let ((source-firstness (make-firstness 
                           'ground-of-being
                           'pure-potentiality))
        (source-secondness (make-secondness
                            'undifferentiated-reality
                            'infinite-actuality))
        (source-thirdness (make-thirdness
                           'creative-principle
                           'universal-law)))
    `(zeroth-order-pattern
      (name . source)
      (order . 0)
      (description . "The foundational ground from which all patterns emerge")
      (semiotic-structure . ,(make-semiotic-cycle 
                              'ground-of-being 
                              'undifferentiated-reality 
                              'creative-principle))
      (aspects . (
        ;; Source-Source: The void, the ground
        (source-source 
          (name . void)
          (alt-name . ground-source)
          (description . "The foundational ground of universal matter/energy consciousness")
          (role . "Signify consciousness necessary for order")
          (principle . "Conscious order"))
        
        ;; Source-Dynamics: Energy, transformation
        (source-dynamics
          (name . energy)
          (alt-name . ground-transformations)
          (description . "Fundamental animating force of reality")
          (role . "Drive dynamic change and transformation")
          (principle . "Dynamic transformation"))
        
        ;; Source-Creativity: Pattern, order
        (source-creativity
          (name . pattern)
          (alt-name . dynamic-order)
          (description . "Repeated types of order")
          (role . "Provide successful templates of systemic design")
          (principle . "Potential and restrictiveness of form"))
        
        ;; Source-Exchange: Power, work rate
        (source-exchange
          (name . power)
          (alt-name . work-rate)
          (description . "Productivity per unit time")
          (role . "Provide systemic productivity")
          (principle . "Productivity rates"))
        
        ;; Source-Structure: Transformity, order accrual
        (source-structure
          (name . transformity)
          (alt-name . order-accrual)
          (description . "Qualitative complexification")
          (role . "Bring advantages of structural increases in order")
          (principle . "Complexity creation"))
        
        ;; Source-Polarity: Resource, source distinctions
        (source-polarity
          (name . resource)
          (alt-name . source-distinctions)
          (description . "Process of transformation of matter/energy")
          (role . "Provide different qualities of matter/energy")
          (principle . "Source transformations"))
        
        ;; Source-Rhythm: Autopoiesis, self creation
        (source-rhythm
          (name . autopoiesis)
          (alt-name . self-creation)
          (description . "Ongoing self-authoring")
          (role . "Maintain unique system through creative growth")
          (principle . "Creative existence")))))))

;;; ============================================================================
;;; FIRST-ORDER PATTERNS (Patterns 1-6)
;;; The six fundamental pattern domains
;;; ============================================================================

;;; Pattern 1: RHYTHM - Temporal patterns, cycles, repetition
(define (make-rhythm-pattern)
  `(first-order-pattern
    (name . rhythm)
    (order . 1)
    (description . "Temporal patterns, cycles, and repetition in time and space")
    (firstness . ,(make-firstness 'temporal-quality 'rhythmic-feeling))
    (secondness . ,(make-secondness 'cyclical-existence 'actual-repetition))
    (thirdness . ,(make-thirdness 'rhythmic-law 'periodic-habit))
    (domain . temporality)
    (perspective-integration . (1st-person-time 2nd-person-cadence 3rd-person-periodicity))
    (principle . "Temporal organization through recurrence")))

;;; Pattern 2: POLARITY - Dualities, oppositions, complementarities
(define (make-polarity-pattern)
  `(first-order-pattern
    (name . polarity)
    (order . 2)
    (description . "Fundamental dualities and complementary oppositions")
    (firstness . ,(make-firstness 'polar-quality 'dualistic-feeling))
    (secondness . ,(make-secondness 'oppositional-existence 'actual-duality))
    (thirdness . ,(make-thirdness 'complementary-law 'dialectical-habit))
    (domain . differentiation)
    (perspective-integration . (1st-person-distinction 2nd-person-relation 3rd-person-opposition))
    (principle . "Creative tension through complementary opposition")))

;;; Pattern 3: STRUCTURE - Forms, boundaries, hierarchies
(define (make-structure-pattern)
  `(first-order-pattern
    (name . structure)
    (order . 3)
    (description . "Organizational forms, boundaries, and spatial arrangements")
    (firstness . ,(make-firstness 'structural-quality 'formal-feeling))
    (secondness . ,(make-secondness 'organized-existence 'actual-form))
    (thirdness . ,(make-thirdness 'organizational-law 'structural-habit))
    (domain . spatiality)
    (perspective-integration . (1st-person-space 2nd-person-organization 3rd-person-architecture))
    (principle . "Spatial organization through form")))

;;; Pattern 4: EXCHANGE - Flows, trades, reciprocities
(define (make-exchange-pattern)
  `(first-order-pattern
    (name . exchange)
    (order . 4)
    (description . "Flows, trades, and reciprocal exchanges between elements")
    (firstness . ,(make-firstness 'flow-quality 'exchange-feeling))
    (secondness . ,(make-secondness 'trading-existence 'actual-exchange))
    (thirdness . ,(make-thirdness 'reciprocal-law 'exchange-habit))
    (domain . relationality)
    (perspective-integration . (1st-person-giving 2nd-person-trading 3rd-person-economy))
    (principle . "Relational coordination through reciprocity")))

;;; Pattern 5: CREATIVITY - Emergence, novelty, innovation
(define (make-creativity-pattern)
  `(first-order-pattern
    (name . creativity)
    (order . 5)
    (description . "Emergence of novelty, innovation, and creative transformation")
    (firstness . ,(make-firstness 'creative-quality 'novel-feeling))
    (secondness . ,(make-secondness 'emergent-existence 'actual-novelty))
    (thirdness . ,(make-thirdness 'innovative-law 'creative-habit))
    (domain . generativity)
    (perspective-integration . (1st-person-inspiration 2nd-person-innovation 3rd-person-evolution))
    (principle . "Developmental transformation through novelty")))

;;; Pattern 6: DYNAMICS - Change, feedback, systems
(define (make-dynamics-pattern)
  `(first-order-pattern
    (name . dynamics)
    (order . 6)
    (description . "Dynamic processes, feedback loops, and systemic integration")
    (firstness . ,(make-firstness 'dynamic-quality 'process-feeling))
    (secondness . ,(make-secondness 'changing-existence 'actual-process))
    (thirdness . ,(make-thirdness 'systemic-law 'dynamic-habit))
    (domain . processuality)
    (perspective-integration . (1st-person-change 2nd-person-feedback 3rd-person-system))
    (principle . "Process integration through feedback")))

;;; ============================================================================
;;; SECOND-ORDER PATTERNS (Patterns 7-48)
;;; 7 major aspects × 6 minor aspects = 42 patterns
;;; Each first-order pattern generates 6 sub-patterns
;;; ============================================================================

;;; Helper function to create a second-order pattern
(define (make-second-order-pattern major minor name alt-name description role principle)
  `(second-order-pattern
    (major-aspect . ,major)
    (minor-aspect . ,minor)
    (name . ,name)
    (alt-name . ,alt-name)
    (order . 2)
    (description . ,description)
    (firstness . ,(make-firstness name 'specific-quality))
    (secondness . ,(make-secondness name 'specific-actuality))
    (thirdness . ,(make-thirdness name 'specific-law))
    (role . ,role)
    (principle . ,principle)
    (holarchical-position . (,major ,minor))))

;;; ============================================================================
;;; SOURCE SECOND-ORDER PATTERNS (6 patterns)
;;; Source × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}
;;; ============================================================================

(define source-dynamics-pattern
  (make-second-order-pattern 'source 'dynamics
    'energy 'ground-transformations
    "The fundamental animating force of reality - rhythmic vibration in space/time"
    "Drive dynamic change and transformation processes"
    "Dynamic transformation: balance energy use with availability"))

(define source-creativity-pattern
  (make-second-order-pattern 'source 'creativity
    'pattern 'dynamic-order
    "Repeated types of order - recurrent design arrangements"
    "Provide successful templates of systemic design"
    "Potential and restrictiveness of form: balance patterned forms with unrestricted states"))

(define source-exchange-pattern
  (make-second-order-pattern 'source 'exchange
    'power 'work-rate
    "Productivity per unit time - rate and efficiency of systemic functioning"
    "Provide systemic productivity"
    "Productivity rates: balance work rate with resource flow"))

(define source-structure-pattern
  (make-second-order-pattern 'source 'structure
    'transformity 'order-accrual
    "Qualitative complexification - transformation to higher complexity"
    "Bring advantages of structural increases in order"
    "Complexity creation: balance transformity with available energy"))

(define source-polarity-pattern
  (make-second-order-pattern 'source 'polarity
    'resource 'source-distinctions
    "Process of transformation of matter/energy - conservation through transformation"
    "Provide different qualities of matter/energy for unique purposes"
    "Source transformations: balance number of transformations with usefulness"))

(define source-rhythm-pattern
  (make-second-order-pattern 'source 'rhythm
    'autopoiesis 'self-creation
    "Ongoing self-authoring - maintenance and creative unfolding"
    "Maintain unique system of order through creative growth"
    "Creative existence: balance self-development with self-maintenance"))

;;; ============================================================================
;;; DYNAMICS SECOND-ORDER PATTERNS (6 patterns)
;;; Dynamics × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}
;;; ============================================================================

(define dynamics-dynamics-pattern
  (make-second-order-pattern 'dynamics 'dynamics
    'system 'integrated-dynamics
    "Ordered activity of a synergistic whole - integrated group of parts"
    "Maintain complex dynamic order"
    "Dynamic order: balance complex dynamic order with randomness"))

(define dynamics-creativity-pattern
  (make-second-order-pattern 'dynamics 'creativity
    'spontaneity 'dynamic-response
    "Coordinated impromptu reaction - instantaneous improvisation"
    "Provide seamless dynamic adaptation"
    "Impromptu adaptations: balance spontaneous reactions with perturbations"))

(define dynamics-exchange-pattern
  (make-second-order-pattern 'dynamics 'exchange
    'feedback 'adjustment-dynamics
    "Dynamic adjustments through causal loops"
    "Make dynamic adjustments"
    "Causal loops: balance amount of feedback with size of adjustments"))

(define dynamics-structure-pattern
  (make-second-order-pattern 'dynamics 'structure
    'synergy 'dynamical-structures
    "Benefit through combination - whole greater than parts"
    "Provide a functional multiplier"
    "Structural integration: balance synergies with diversity of function"))

(define dynamics-polarity-pattern
  (make-second-order-pattern 'dynamics 'polarity
    'agency-communion 'part-whole-polarity
    "Tension between integration and disintegration"
    "Create and dissolve systems"
    "Formative boundaries: balance boundary formation with dissolution"))

(define dynamics-rhythm-pattern
  (make-second-order-pattern 'dynamics 'rhythm
    'iterate 'dynamic-rhythm
    "Repeated cycles - incremental change through recurrent cycling"
    "Support ongoing incremental adaptations"
    "Cyclic adaptations: balance iterative speed with rate of change"))

;;; ============================================================================
;;; CREATIVITY SECOND-ORDER PATTERNS (6 patterns)
;;; Creativity × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}
;;; ============================================================================

(define creativity-dynamics-pattern
  (make-second-order-pattern 'creativity 'dynamics
    'evolution 'dynamic-creativity
    "Leap to a higher level of complexity - qualitative transformation"
    "Make developmental leaps"
    "Creative leaps: balance shifts to higher levels with foundational adaptations"))

(define creativity-creativity-pattern
  (make-second-order-pattern 'creativity 'creativity
    'emergence 'innovative-arising
    "Moment of creative development - appearance of novel forms"
    "Launch new patterns of organization"
    "Innovative arising: balance emergence of novelty with energy for fusion"))

(define creativity-exchange-pattern
  (make-second-order-pattern 'creativity 'exchange
    'growth 'creative-prosperity
    "Developmental increase - compounding additions through cycles"
    "Create resources for system building"
    "Compound prosperity: balance exponential increase with resource availability"))

(define creativity-structure-pattern
  (make-second-order-pattern 'creativity 'structure
    'adaptation 'structural-adjustments
    "Structural alterations within existing frameworks"
    "Adjust existing structures"
    "Structural modifications: balance modifications with functional integrity"))

(define creativity-polarity-pattern
  (make-second-order-pattern 'creativity 'polarity
    'bifurcation 'liminal-creation
    "Point at which state change takes place - breakdown to new order"
    "Generate new forms of order from breakdown"
    "Creative chaos: balance depth of chaos with value of new order"))

(define creativity-rhythm-pattern
  (make-second-order-pattern 'creativity 'rhythm
    'seed 'emergent-creation
    "Repeated emergence of creative beginnings"
    "Emerge new opportunities"
    "Viable germination: balance number of seeds with their viability"))

;;; ============================================================================
;;; EXCHANGE SECOND-ORDER PATTERNS (6 patterns)
;;; Exchange × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}
;;; ============================================================================

(define exchange-dynamics-pattern
  (make-second-order-pattern 'exchange 'dynamics
    'process 'operation-dynamics
    "Linear stage-by-stage development through sequence of steps"
    "Create value through sequential development"
    "Developmental process: balance number of steps with value creation"))

(define exchange-creativity-pattern
  (make-second-order-pattern 'exchange 'creativity
    'uniqueness 'exchange-creation
    "Difference between elements - variations creating distinctness"
    "Provide differentiation in form and function"
    "Essential distinctions: balance differentiation with similarity"))

(define exchange-exchange-pattern
  (make-second-order-pattern 'exchange 'exchange
    'trade 'essence-of-exchange
    "Simple reciprocation - reciprocal exchange of unique resources"
    "Improve productivity through specialization"
    "Reciprocity: balance exchange of different resources"))

(define exchange-structure-pattern
  (make-second-order-pattern 'exchange 'structure
    'capture 'flow-container
    "Structure required to obtain yield from flow outside system"
    "Acquire resources for the system"
    "Obtaining a yield: balance capture of flows with structural capacity"))

(define exchange-polarity-pattern
  (make-second-order-pattern 'exchange 'polarity
    'balance 'relational-duality
    "Dynamic equilibrium of relational reciprocity"
    "Generate systemic energy through exchanges"
    "Equitable exchange: balance giving with receiving"))

(define exchange-rhythm-pattern
  (make-second-order-pattern 'exchange 'rhythm
    'cycle 'exchange-phases
    "Circuit of phases in interchange processes"
    "Provide sequence of repeated phases in any process"
    "Cyclic exchanges: balance phases in iterative circuit"))

;;; ============================================================================
;;; STRUCTURE SECOND-ORDER PATTERNS (6 patterns)
;;; Structure × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}
;;; ============================================================================

(define structure-dynamics-pattern
  (make-second-order-pattern 'structure 'dynamics
    'holarchy 'structural-dynamics
    "Nested arrangement of systems within systems"
    "Maintain order in dynamic growth of complexity"
    "Nested levels: balance number of levels with systems at each level"))

(define structure-creativity-pattern
  (make-second-order-pattern 'structure 'creativity
    'complexity 'order-creation
    "Number of elements and connections in a system"
    "Creatively configure high degrees of order"
    "Interrelationships: balance numerous unique elements with simplicity"))

(define structure-exchange-pattern
  (make-second-order-pattern 'structure 'exchange
    'network 'relational-design
    "Inter-connective architecture of relationships - nodes and pathways"
    "Provide organizational clarity through interconnected distinctions"
    "Nodes and pathways: balance strength of relationships with node integrity"))

(define structure-structure-pattern
  (make-second-order-pattern 'structure 'structure
    'hierarchy 'essential-structure
    "Essential property of system design - ranking of levels"
    "Generate gain through concentrated control supporting base"
    "Control as service: balance concentrated control with diffuse influence"))

(define structure-polarity-pattern
  (make-second-order-pattern 'structure 'polarity
    'holon 'part-whole
    "Fundamental structural duality between parts and wholes"
    "Illustrate defining structural relationship of systems"
    "Part and whole: balance system's role as part with identity as whole"))

(define structure-rhythm-pattern
  (make-second-order-pattern 'structure 'rhythm
    'boundary 'edge-pattern
    "Design of the limiting edge of a system"
    "Maintain distinctions"
    "Edge design: balance structural definition with permeability"))

;;; ============================================================================
;;; POLARITY SECOND-ORDER PATTERNS (6 patterns)
;;; Polarity × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}
;;; ============================================================================

(define polarity-dynamics-pattern
  (make-second-order-pattern 'polarity 'dynamics
    'competition-cooperation 'dynamic-polarity
    "Fundamental duality at systems level - striving vs collaboration"
    "Enhance functionality at systems level"
    "Competitive cooperation: balance competitive striving with cooperative synergies"))

(define polarity-creativity-pattern
  (make-second-order-pattern 'polarity 'creativity
    'order-chaos 'creative-polarity
    "Oppositional dynamics in creative process"
    "Facilitate adaptation and evolution"
    "Creative breakdown: balance structured function with breakdown"))

(define polarity-exchange-pattern
  (make-second-order-pattern 'polarity 'exchange
    'flows-stores 'exchange-states
    "Dualistic form of exchangeable resources"
    "Ensure uninterrupted capacity for exchanges"
    "Augmented flows: balance resources in flow with resources in stocks"))

(define polarity-structure-pattern
  (make-second-order-pattern 'polarity 'structure
    'input-output 'polarity-structure
    "Dualism in systemic structuring - resources needed vs wastes emitted"
    "Process resources"
    "Waste resources: balance input of resources with output of wastes"))

(define polarity-polarity-pattern
  (make-second-order-pattern 'polarity 'polarity
    'concentration-diffusion 'primordial-duality
    "Foundational duality within systems - concentrated centers vs diffuse areas"
    "Leverage advantages of intensive centers"
    "Complex interconnections: balance flow to centers with distribution to hinterlands"))

(define polarity-rhythm-pattern
  (make-second-order-pattern 'polarity 'rhythm
    'expand-contract 'rhythmic-duality
    "Fundamental duality in any rhythmic movement or event"
    "Liberate energy through rhythmic interplay"
    "Rhythmic interplay: balance expansive movement with contractive movement"))

;;; ============================================================================
;;; RHYTHM SECOND-ORDER PATTERNS (6 patterns)
;;; Rhythm × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}
;;; ============================================================================

(define rhythm-dynamics-pattern
  (make-second-order-pattern 'rhythm 'dynamics
    'enantiodromia 'emergent-oppositions
    "Force exerted by extreme movements on emergence of opposites"
    "Birth emergence of corrective dynamics"
    "Emergent opposites: balance energy of corrective dynamics to be generative"))

(define rhythm-creativity-pattern
  (make-second-order-pattern 'rhythm 'creativity
    'synchronization 'mesh-works
    "Creative inter-meshing of elements and processes in time"
    "Find optimal arrangements of interconnections"
    "Creative interconnections: balance creative arrangements with stable functioning"))

(define rhythm-exchange-pattern
  (make-second-order-pattern 'rhythm 'exchange
    'pulse 'flow-surges
    "Repeated rhythmic surges of activity in resource flows"
    "Maximize exchange flows sustainably"
    "Peaks: balance rate of increase with rate of decline"))

(define rhythm-structure-pattern
  (make-second-order-pattern 'rhythm 'structure
    'cadence 'rhythm-structures
    "Structuring of rhythms within systems - complex temporal coordination"
    "Provide complex rhythmic programming"
    "Complex timing: balance rhythmic complexity with simplicity"))

(define rhythm-polarity-pattern
  (make-second-order-pattern 'rhythm 'polarity
    'swing 'iterative-extremes
    "Repeated movement toward one pole then back to opposite"
    "Maintain dynamic stability"
    "Dynamic stability: balance size of swings with frequency of adjustments"))

(define rhythm-rhythm-pattern
  (make-second-order-pattern 'rhythm 'rhythm
    'repetition 'recurrent-order
    "Simple ongoing recurrences in time and space"
    "Provide reliability"
    "Simple recurrence: balance reliability of repetition with need for variation"))

;;; ============================================================================
;;; COMPLETE 49-PATTERN SYSTEM
;;; ============================================================================

(define (make-49-pattern-holarchy)
  `(pattern-holarchy
    (total-patterns . 49)
    (structure . (
      (zeroth-order . 1)
      (first-order . 6)
      (second-order . 42)))
    
    ;; Zeroth-order: Source (1 pattern)
    (zeroth-order-patterns . (
      ,(make-source-pattern)))
    
    ;; First-order: 6 fundamental patterns
    (first-order-patterns . (
      ,(make-rhythm-pattern)
      ,(make-polarity-pattern)
      ,(make-structure-pattern)
      ,(make-exchange-pattern)
      ,(make-creativity-pattern)
      ,(make-dynamics-pattern)))
    
    ;; Second-order: 42 patterns (7 major × 6 minor)
    (second-order-patterns . (
      ;; Source family (6 patterns)
      (source-family . (
        ,source-dynamics-pattern
        ,source-creativity-pattern
        ,source-exchange-pattern
        ,source-structure-pattern
        ,source-polarity-pattern
        ,source-rhythm-pattern))
      
      ;; Dynamics family (6 patterns)
      (dynamics-family . (
        ,dynamics-dynamics-pattern
        ,dynamics-creativity-pattern
        ,dynamics-exchange-pattern
        ,dynamics-structure-pattern
        ,dynamics-polarity-pattern
        ,dynamics-rhythm-pattern))
      
      ;; Creativity family (6 patterns)
      (creativity-family . (
        ,creativity-dynamics-pattern
        ,creativity-creativity-pattern
        ,creativity-exchange-pattern
        ,creativity-structure-pattern
        ,creativity-polarity-pattern
        ,creativity-rhythm-pattern))
      
      ;; Exchange family (6 patterns)
      (exchange-family . (
        ,exchange-dynamics-pattern
        ,exchange-creativity-pattern
        ,exchange-exchange-pattern
        ,exchange-structure-pattern
        ,exchange-polarity-pattern
        ,exchange-rhythm-pattern))
      
      ;; Structure family (6 patterns)
      (structure-family . (
        ,structure-dynamics-pattern
        ,structure-creativity-pattern
        ,structure-exchange-pattern
        ,structure-structure-pattern
        ,structure-polarity-pattern
        ,structure-rhythm-pattern))
      
      ;; Polarity family (6 patterns)
      (polarity-family . (
        ,polarity-dynamics-pattern
        ,polarity-creativity-pattern
        ,polarity-exchange-pattern
        ,polarity-structure-pattern
        ,polarity-polarity-pattern
        ,polarity-rhythm-pattern))
      
      ;; Rhythm family (6 patterns)
      (rhythm-family . (
        ,rhythm-dynamics-pattern
        ,rhythm-creativity-pattern
        ,rhythm-exchange-pattern
        ,rhythm-structure-pattern
        ,rhythm-polarity-pattern
        ,rhythm-rhythm-pattern))))
    
    (nondual-origin . ,nondual-origin)
    (framework . integral-semiotic-realism)
    (integration . pattern-dynamics)))

;;; ============================================================================
;;; UTILITY FUNCTIONS
;;; ============================================================================

;;; Get all patterns as a flat list
(define (get-all-49-patterns)
  (let* ((holarchy (make-49-pattern-holarchy))
         (second-order (assoc-ref holarchy 'second-order-patterns))
         (all-second-order-patterns
           (apply append
             (map (lambda (family)
                    (cdr family))
                  second-order))))
    (append
      (assoc-ref holarchy 'zeroth-order-patterns)
      (assoc-ref holarchy 'first-order-patterns)
      all-second-order-patterns)))

;;; Get patterns by order
(define (get-patterns-by-order order)
  (let ((holarchy (make-49-pattern-holarchy)))
    (cond
      ((= order 0) (assoc-ref holarchy 'zeroth-order-patterns))
      ((= order 1) (assoc-ref holarchy 'first-order-patterns))
      ((= order 2) (assoc-ref holarchy 'second-order-patterns))
      (else '()))))

;;; Get patterns by major aspect
(define (get-patterns-by-major-aspect aspect)
  (let* ((holarchy (make-49-pattern-holarchy))
         (second-order (assoc-ref holarchy 'second-order-patterns))
         (family-key (string->symbol (string-append (symbol->string aspect) "-family"))))
    (assoc-ref second-order family-key)))

;;; Display pattern summary
(define (display-pattern-summary pattern)
  (let ((name (assoc-ref pattern 'name))
        (description (assoc-ref pattern 'description))
        (order (assoc-ref pattern 'order)))
    (display (format #f "~a (Order ~a): ~a~%" name order description))))

;;; Display complete holarchy structure
(define (display-49-pattern-holarchy)
  (let ((holarchy (make-49-pattern-holarchy)))
    (display "\n=== 49 PATTERN DYNAMICS HOLARCHY ===\n\n")
    
    (display "ZEROTH-ORDER (1 pattern):\n")
    (for-each display-pattern-summary 
              (assoc-ref holarchy 'zeroth-order-patterns))
    
    (display "\nFIRST-ORDER (6 patterns):\n")
    (for-each display-pattern-summary 
              (assoc-ref holarchy 'first-order-patterns))
    
    (display "\nSECOND-ORDER (42 patterns by family):\n")
    (let ((second-order (assoc-ref holarchy 'second-order-patterns)))
      (for-each
        (lambda (family)
          (let ((family-name (car family))
                (patterns (cdr family)))
            (display (format #f "\n~a (~a patterns):\n" family-name (length patterns)))
            (for-each display-pattern-summary patterns)))
        second-order))
    
    (display "\n=== END OF HOLARCHY ===\n\n")))

;;; Export for use by other modules
(define pattern-dynamics-49
  (make-49-pattern-holarchy))
