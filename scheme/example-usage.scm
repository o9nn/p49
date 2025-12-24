#!/usr/bin/env guile
!#

;;; example-usage.scm
;;;
;;; Quick example showing how to use the Pattern Archetype

(load "pattern-archetype.scm")

(display "")
(newline)
(display "═══════════════════════════════════════════════════════════════")
(newline)
(display " INTEGRAL SEMIOTIC REALISM - QUICK EXAMPLE")
(newline)
(display "═══════════════════════════════════════════════════════════════")
(newline)
(newline)

;; Example: Understanding how we perceive a red apple
(display "EXAMPLE: Visual Perception of a Red Apple")
(newline)
(newline)

;; Create the three semiotic categories
(display "1. FIRSTNESS (The Sign) - Immediate sensory experience:")
(newline)
(define red-sensation 
  (make-firstness 'red-round-visual-quality 'immediate-color-shape))
(display-component red-sensation)
(newline)

(display "2. SECONDNESS (The Object) - The actual physical apple:")
(newline)
(define physical-apple
  (make-secondness 'real-apple-object 'brute-physical-existence))
(display-component physical-apple)
(newline)

(display "3. THIRDNESS (The Interpretant) - Our conceptual understanding:")
(newline)
(define apple-concept
  (make-thirdness 'fruit-recognition 'general-apple-category))
(display-component apple-concept)
(newline)

;; Show the complete semiotic cycle
(display "═══════════════════════════════════════════════════════════════")
(newline)
(display " THE COMPLETE SEMIOTIC CYCLE")
(newline)
(display "═══════════════════════════════════════════════════════════════")
(newline)
(newline)

(define perception-cycle
  (make-semiotic-cycle
    'red-visual-sensation    ; Sign: what we immediately experience
    'physical-apple-object   ; Object: the actual apple in the world
    'apple-recognition))     ; Interpretant: our understanding "this is an apple"

(display "The cycle connects:")
(newline)
(display "  • Visual sensation (Firstness)")
(newline)
(display "  → Conceptual recognition (Thirdness)")
(newline)
(display "  → Physical object (Secondness)")
(newline)
(display "  → Back to new sensations (continuous signification)")
(newline)
(newline)

;; Show emergence from NonDual Origin
(display "═══════════════════════════════════════════════════════════════")
(newline)
(display " EMERGENCE FROM NONDUAL ORIGIN")
(newline)
(display "═══════════════════════════════════════════════════════════════")
(newline)
(newline)

(display "Before the distinction of subject and object,")
(newline)
(display "there is the NonDual Origin:")
(newline)
(newline)
(display-component nondual-origin)
(newline)

(display "From this undifferentiated source:")
(newline)
(display "  • EPISTEMIC EMERGENCE → our subjective experience arises")
(newline)
(display "  • ONTOLOGICAL EMERGENCE → the objective world manifests")
(newline)
(newline)

;; Create a complete integral archetype
(display "═══════════════════════════════════════════════════════════════")
(newline)
(display " INTEGRAL ARCHETYPE: PERCEPTION AS PATTERN")
(newline)
(display "═══════════════════════════════════════════════════════════════")
(newline)
(newline)

(define perception-archetype
  (make-integral-archetype
    'sensory-experience        ; Firstness: immediate sensation
    'physical-reality         ; Secondness: actual world
    'perceptual-understanding ; Thirdness: recognition/meaning
    (make-dynamic-pattern
      'perception-pattern
      '(sensorimotor-feedback-loop)
      'adaptive-refinement)))

(display "This archetype integrates:")
(newline)
(display "  ✓ The triadic semiotic cycle")
(newline)
(display "  ✓ Three perspectival domains")
(newline)
(display "  ✓ NonDual origin and evolution")
(newline)
(display "  ✓ Dynamic meta-pattern (sensorimotor loop)")
(newline)
(newline)

(display "═══════════════════════════════════════════════════════════════")
(newline)
(display " KEY INSIGHT")
(newline)
(display "═══════════════════════════════════════════════════════════════")
(newline)
(newline)
(display "Perception is not just:")
(newline)
(display "  • Sensation alone (1st person only)")
(newline)
(display "  • Physical causation alone (3rd person only)")
(newline)
(newline)
(display "It requires the INTEGRAL pattern:")
(newline)
(display "  • 1st person: The 'what it's like' of seeing red")
(newline)
(display "  • 2nd person: The shared meaning 'apple'")
(newline)
(display "  • 3rd person: The objective physical apple")
(newline)
(display "  • All arising from NonDual Origin")
(newline)
(newline)
(display "═══════════════════════════════════════════════════════════════")
(newline)
(newline)
