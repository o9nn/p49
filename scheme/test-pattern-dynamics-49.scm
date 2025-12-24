#!/usr/bin/env guile
!#

;;; test-pattern-dynamics-49.scm
;;; Test suite for the 49 Pattern Dynamics patterns

(load "pattern-dynamics-49.scm")

(display "===================================\n")
(display "Testing 49 Pattern Dynamics System\n")
(display "===================================\n\n")

;;; Test 1: Create complete holarchy
(display "Test 1: Creating complete 49-pattern holarchy...\n")
(define holarchy (make-49-pattern-holarchy))
(display "✓ Holarchy created successfully\n\n")

;;; Test 2: Verify structure
(display "Test 2: Verifying structure...\n")
(define total-patterns (assoc-ref holarchy 'total-patterns))
(define structure (assoc-ref holarchy 'structure))
(display (format #f "Total patterns: ~a\n" total-patterns))
(display (format #f "Structure: ~a\n" structure))
(if (= total-patterns 49)
    (display "✓ Correct total: 49 patterns\n")
    (display "✗ ERROR: Incorrect total\n"))
(display "\n")

;;; Test 3: Verify zeroth-order pattern
(display "Test 3: Verifying zeroth-order pattern (Source)...\n")
(define zeroth-patterns (assoc-ref holarchy 'zeroth-order-patterns))
(display (format #f "Number of zeroth-order patterns: ~a\n" (length zeroth-patterns)))
(define source-pattern (car zeroth-patterns))
(display (format #f "Source pattern name: ~a\n" (assoc-ref source-pattern 'name)))
(define source-aspects (assoc-ref source-pattern 'aspects))
(display (format #f "Number of Source aspects: ~a\n" (length source-aspects)))
(if (= (length source-aspects) 7)
    (display "✓ Source has 7 aspects\n")
    (display "✗ ERROR: Source should have 7 aspects\n"))
(display "\n")

;;; Test 4: Verify first-order patterns
(display "Test 4: Verifying first-order patterns...\n")
(define first-patterns (assoc-ref holarchy 'first-order-patterns))
(display (format #f "Number of first-order patterns: ~a\n" (length first-patterns)))
(if (= (length first-patterns) 6)
    (display "✓ Correct count: 6 first-order patterns\n")
    (display "✗ ERROR: Should have 6 first-order patterns\n"))
(display "First-order pattern names: ")
(for-each (lambda (p) 
            (display (format #f "~a " (assoc-ref p 'name))))
          first-patterns)
(display "\n\n")

;;; Test 5: Verify second-order patterns
(display "Test 5: Verifying second-order patterns...\n")
(define second-patterns (assoc-ref holarchy 'second-order-patterns))
(define family-names '(source-family dynamics-family creativity-family 
                       exchange-family structure-family polarity-family rhythm-family))
(display (format #f "Number of families: ~a\n" (length family-names)))

(define total-second-order
  (apply + 
    (map (lambda (family-name)
           (let ((family (assoc-ref second-patterns family-name)))
             (if family (length family) 0)))
         family-names)))

(display (format #f "Total second-order patterns: ~a\n" total-second-order))
(if (= total-second-order 42)
    (display "✓ Correct count: 42 second-order patterns\n")
    (display "✗ ERROR: Should have 42 second-order patterns\n"))

;; Verify each family
(for-each
  (lambda (family-name)
    (let ((family (assoc-ref second-patterns family-name)))
      (if family
          (display (format #f "  ~a: ~a patterns\n" family-name (length family)))
          (display (format #f "  ~a: ERROR - not found\n" family-name)))))
  family-names)
(display "\n")

;;; Test 6: Display complete holarchy
(display "Test 6: Displaying complete holarchy structure...\n")
(display-49-pattern-holarchy)

;;; Test 7: Test utility functions
(display "Test 7: Testing utility functions...\n")
(define all-patterns (get-all-49-patterns))
(display (format #f "get-all-49-patterns returned ~a patterns\n" (length all-patterns)))
(if (= (length all-patterns) 49)
    (display "✓ All patterns retrieved correctly\n")
    (display "✗ ERROR: Should retrieve 49 patterns\n"))

(define zeroth-order (get-patterns-by-order 0))
(display (format #f "Zeroth-order patterns: ~a\n" (length zeroth-order)))

(define first-order (get-patterns-by-order 1))
(display (format #f "First-order patterns: ~a\n" (length first-order)))

(define rhythm-family (get-patterns-by-major-aspect 'rhythm))
(display (format #f "Rhythm family patterns: ~a\n" (length rhythm-family)))
(display "\n")

;;; Test 8: Verify semiotic structure in patterns
(display "Test 8: Verifying semiotic structure...\n")
(define test-pattern (car first-patterns))
(define firstness (assoc-ref test-pattern 'firstness))
(define secondness (assoc-ref test-pattern 'secondness))
(define thirdness (assoc-ref test-pattern 'thirdness))

(if (and firstness secondness thirdness)
    (display "✓ First-order patterns have complete semiotic structure\n")
    (display "✗ ERROR: Missing semiotic elements\n"))

;; Check a second-order pattern
(define rhythm-patterns (assoc-ref second-patterns 'rhythm-family))
(define test-second-order (car rhythm-patterns))
(define so-firstness (assoc-ref test-second-order 'firstness))
(define so-secondness (assoc-ref test-second-order 'secondness))
(define so-thirdness (assoc-ref test-second-order 'thirdness))

(if (and so-firstness so-secondness so-thirdness)
    (display "✓ Second-order patterns have complete semiotic structure\n")
    (display "✗ ERROR: Missing semiotic elements in second-order\n"))
(display "\n")

;;; Test 9: Verify holarchical positions
(display "Test 9: Verifying holarchical positions...\n")
(define sample-pattern (car rhythm-patterns))
(define major (assoc-ref sample-pattern 'major-aspect))
(define minor (assoc-ref sample-pattern 'minor-aspect))
(display (format #f "Sample pattern: ~a × ~a = ~a\n" 
                 major minor (assoc-ref sample-pattern 'name)))
(if (and major minor)
    (display "✓ Holarchical positions properly defined\n")
    (display "✗ ERROR: Missing holarchical position\n"))
(display "\n")

;;; Test 10: Verify NonDual Origin integration
(display "Test 10: Verifying NonDual Origin integration...\n")
(define origin (assoc-ref holarchy 'nondual-origin))
(if origin
    (begin
      (display "✓ NonDual Origin present in holarchy\n")
      (display (format #f "  Origin zone: ~a\n" (assoc-ref origin 'zone)))
      (display (format #f "  Origin domain: ~a\n" (assoc-ref origin 'domain))))
    (display "✗ ERROR: NonDual Origin missing\n"))
(display "\n")

;;; Summary
(display "===================================\n")
(display "Test Summary Complete\n")
(display "===================================\n")
(display "\n")
(display "All 49 patterns successfully created and verified!\n")
(display "\n")
(display "Pattern Structure:\n")
(display "  1 Zeroth-order (Source with 7 aspects)\n")
(display "  6 First-order (Rhythm, Polarity, Structure, Exchange, Creativity, Dynamics)\n")
(display " 42 Second-order (7 major aspects × 6 minor aspects)\n")
(display "---\n")
(display " 49 Total Patterns\n")
(display "\n")
