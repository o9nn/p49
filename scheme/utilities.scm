;;; Additional Utilities for Pattern Primary Archetype
;;; Advanced querying, visualization, and analysis functions

(load "pattern-primary-archetype.scm")

;; ============================================================================
;; Graph Visualization (ASCII Art)
;; ============================================================================

;;; Display the infinity loop structure as ASCII art
(define (display-infinity-loop)
  (display "
                      Thirdness/Interpretant
                      2nd Person Perspective
                      [methodology]
                           /    \\
                          /      \\
            continuous  /        \\  continuous
            signification        signification
           (method)   /            \\   (method)
                     /              \\
                    v                v
         Firstness/Sign          Secondness/Object
         1st Person              3rd Person
         [epistemology]          [ontology]
                 \\                  /
       epistemic  \\                /  ontological
       emergence   \\              /   emergence
                    \\            /
                     \\          /
                      v        v
                      
                   [Actual Domain]
                   
                        | |
                        | |
                   nondual return
                        | |
                        v v
                        
                  NonDual Origin
                [Intransitive Domain]
                [Zone of Subsistence]
                        
                        | |
                        | |
                        v v
                        
                     Enactment
                  [Empirical Domain]
                  [Zone of Existence]
") (newline))

;; ============================================================================
;; Advanced Query Functions
;; ============================================================================

;;; Find all nodes in a specific domain
(define (nodes-in-domain domain nodes)
  (filter (lambda (node)
            (equal? (domain-name (node-domain node))
                   (domain-name domain)))
          nodes))

;;; Find all processes using a specific method
(define (processes-by-method method processes)
  (filter (lambda (proc)
            (eq? (process-method proc) method))
          processes))

;;; Get all properties of a specific aspect from all nodes
(define (collect-property-values property nodes)
  (map (lambda (node)
         (cons (node-name node)
               (node-property node property)))
       nodes))

;;; Find the path between two nodes through processes
(define (find-path start-node end-node processes max-depth)
  (define (find-path-helper current target depth path visited)
    (cond
      ((= depth 0) #f)
      ((equal? (node-name current) (node-name target))
       (reverse (cons current path)))
      ((member (node-name current) visited) #f)
      (else
       (let ((outgoing (processes-from-node current processes)))
         (let loop ((procs outgoing))
           (cond
             ((null? procs) #f)
             (else
              (let ((next (process-target (car procs))))
                (let ((result (find-path-helper
                              next
                              target
                              (- depth 1)
                              (cons current path)
                              (cons (node-name current) visited))))
                  (if result
                      result
                      (loop (cdr procs))))))))))))
  (find-path-helper start-node end-node max-depth '() '()))

;; ============================================================================
;; Pattern Analysis Functions
;; ============================================================================

;;; Analyze the cyclic structure
(define (analyze-cycles start-node max-cycles)
  (define (analyze-helper node cycles-remaining results)
    (if (= cycles-remaining 0)
        results
        (let ((cycle-path (traverse-cycle node 4)))
          (analyze-helper node
                        (- cycles-remaining 1)
                        (cons cycle-path results)))))
  (reverse (analyze-helper start-node max-cycles '())))

;;; Count processes by domain transition
(define (count-domain-transitions processes)
  (define (update-count transition counts)
    (let ((existing (assoc transition counts)))
      (if existing
          (cons (cons transition (+ 1 (cdr existing)))
                (filter (lambda (p) (not (equal? (car p) transition))) counts))
          (cons (cons transition 1) counts))))
  
  (let loop ((procs processes) (transitions '()))
    (if (null? procs)
        transitions
        (let* ((proc (car procs))
               (src-domain (domain-name (node-domain (process-source proc))))
               (tgt-domain (domain-name (node-domain (process-target proc))))
               (transition (cons src-domain tgt-domain)))
          (loop (cdr procs)
                (update-count transition transitions))))))

;; ============================================================================
;; Relational Mapping Functions
;; ============================================================================

;;; Map perspectives to their corresponding nodes
(define (perspective-node-mapping)
  (list
   (cons 'first-person firstness)
   (cons 'second-person thirdness)
   (cons 'third-person secondness)
   (cons 'pre-perspectival nondual-origin)
   (cons 'multi-perspectival enactment)))

;;; Get all relationships for a node
(define (node-relationships node)
  (list
   (cons 'outgoing (processes-from-node node semiotic-cycle))
   (cons 'incoming (processes-to-node node semiotic-cycle))
   (cons 'domain (node-domain node))
   (cons 'perspective (node-perspective node))))

;; ============================================================================
;; Transformation and Evolution
;; ============================================================================

;;; Apply a series of transformations
(define (apply-transformation-chain data processes)
  (if (null? processes)
      data
      (apply-transformation-chain
       (apply-semiotic-transformation data (car processes))
       (cdr processes))))

;;; Simulate pattern emergence through iteration
(define (simulate-emergence initial-state iterations process)
  (define (iter state count history)
    (if (= count 0)
        (reverse history)
        (let ((new-state (apply-semiotic-transformation state process)))
          (iter new-state
                (- count 1)
                (cons new-state history)))))
  (iter initial-state iterations (list initial-state)))

;; ============================================================================
;; Pretty Printing and Display
;; ============================================================================

;;; Display node relationships in detail
(define (display-node-relationships node)
  (display "Relationships for: ") (display (node-name node)) (newline)
  (let ((rels (node-relationships node)))
    (display "  Outgoing processes: ")
    (display (map process-name (cdr (assoc 'outgoing rels))))
    (newline)
    (display "  Incoming processes: ")
    (display (map process-name (cdr (assoc 'incoming rels))))
    (newline)
    (display "  Domain: ")
    (display (domain-name (cdr (assoc 'domain rels))))
    (newline)
    (display "  Perspective: ")
    (display (perspective-label (cdr (assoc 'perspective rels))))
    (newline)))

;;; Display the complete cycle structure
(define (display-cycle-structure)
  (display "=== SEMIOTIC CYCLE STRUCTURE ===") (newline) (newline)
  (display "Starting from Firstness:") (newline)
  (let ((cycle (traverse-cycle firstness 8)))
    (for-each (lambda (node)
                (display "  -> ")
                (display (node-name node))
                (newline))
              cycle))
  (newline))

;; ============================================================================
;; Statistical Analysis
;; ============================================================================

;;; Count nodes by domain
(define (count-nodes-by-domain)
  (list
   (cons 'actual (length (nodes-in-domain actual-domain all-nodes)))
   (cons 'intransitive (length (nodes-in-domain intransitive-domain all-nodes)))
   (cons 'empirical (length (nodes-in-domain empirical-domain all-nodes)))))

;;; Count processes by method
(define (count-processes-by-method)
  (list
   (cons 'methodological (length (processes-by-method 'methodological semiotic-cycle)))
   (cons 'epistemological (length (processes-by-method 'epistemological semiotic-cycle)))
   (cons 'ontological (length (processes-by-method 'ontological semiotic-cycle)))
   (cons 'integrative (length (processes-by-method 'integrative semiotic-cycle)))))

;; ============================================================================
;; Export and Serialization
;; ============================================================================

;;; Export the framework as a simple data structure
(define (export-framework)
  (list
   (cons 'version "1.0")
   (cons 'name "Pattern Primary Archetype")
   (cons 'type "Integral Semiotic Realism")
   (cons 'nodes (map (lambda (n)
                       (list (node-name n)
                            (node-category n)
                            (perspective-type (node-perspective n))
                            (domain-name (node-domain n))))
                    all-nodes))
   (cons 'processes (map (lambda (p)
                          (list (process-name p)
                               (node-name (process-source p))
                               (node-name (process-target p))
                               (process-method p)))
                        semiotic-cycle))
   (cons 'domains (map domain-name all-domains))
   (cons 'perspectives (map perspective-type all-perspectives))))

;; ============================================================================
;; Example Demonstrations
;; ============================================================================

(define (demonstrate-utilities)
  (display "=== UTILITY FUNCTIONS DEMONSTRATION ===") (newline) (newline)
  
  ;; Show ASCII visualization
  (display "1. INFINITY LOOP STRUCTURE:") (newline)
  (display-infinity-loop)
  
  ;; Show nodes by domain
  (display "2. NODES BY DOMAIN:") (newline)
  (display "   Actual: ")
  (display (map node-name (nodes-in-domain actual-domain all-nodes)))
  (newline)
  (display "   Intransitive: ")
  (display (map node-name (nodes-in-domain intransitive-domain all-nodes)))
  (newline)
  (display "   Empirical: ")
  (display (map node-name (nodes-in-domain empirical-domain all-nodes)))
  (newline) (newline)
  
  ;; Show property collection
  (display "3. ASPECT PROPERTIES OF ALL NODES:") (newline)
  (for-each (lambda (pair)
              (display "   ")
              (display (car pair))
              (display ": ")
              (display (cdr pair))
              (newline))
            (collect-property-values 'aspect all-nodes))
  (newline)
  
  ;; Show node relationships
  (display "4. FIRSTNESS RELATIONSHIPS:") (newline)
  (display-node-relationships firstness)
  (newline)
  
  ;; Show cycle structure
  (display "5. CYCLE STRUCTURE:") (newline)
  (display-cycle-structure)
  
  ;; Show statistics
  (display "6. STATISTICS:") (newline)
  (display "   Nodes by domain: ")
  (display (count-nodes-by-domain))
  (newline)
  (display "   Processes by method: ")
  (display (count-processes-by-method))
  (newline) (newline)
  
  ;; Show framework export
  (display "7. FRAMEWORK EXPORT (simplified):") (newline)
  (display "   ")
  (display (export-framework))
  (newline) (newline)
  
  ;; Show path finding
  (display "8. PATH FROM FIRSTNESS TO NONDUAL-ORIGIN:") (newline)
  (let ((path (find-path firstness nondual-origin semiotic-cycle 5)))
    (if path
        (begin
          (display "   Path found: ")
          (display (map node-name path))
          (newline))
        (begin
          (display "   No path found within depth limit")
          (newline))))
  (newline)
  
  ;; Show emergence simulation
  (display "9. EMERGENCE SIMULATION (3 iterations):") (newline)
  (let ((emergence (simulate-emergence 'initial-phenomenon 3 epistemic-emergence)))
    (display "   Evolution: ") (newline)
    (for-each (lambda (state)
                (display "     -> ")
                (display state)
                (newline))
              emergence))
  (newline)
  
  (display "=== END OF UTILITIES DEMONSTRATION ===") (newline))

;; Run demonstration if loaded directly
(demonstrate-utilities)
