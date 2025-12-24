# Pattern Dynamics 49-Pattern Quick Reference

## Complete Pattern List

### Zeroth-Order (1 Pattern) = (1+6)^0 = 7^0

1. **SOURCE** - The foundational ground from which all patterns emerge

### First-Order (1+6 = 7 Patterns) = (1+6)^1 = 7^1

1-1. **SOURCE** - The foundational ground from which all patterns emerge

1-2. **POLARITY** - Fundamental dualities and complementary oppositions
1-3. **STRUCTURE** - Organizational forms, boundaries, and spatial arrangements
1-4. **EXCHANGE** - Flows, trades, and reciprocal exchanges
1-5. **CREATIVITY** - Emergence of novelty, innovation, and transformation
1-6. **DYNAMICS** - Dynamic processes, feedback loops, and systemic integration
1-7. **RHYTHM** - Temporal patterns, cycles, and repetition

### Second-Order (1+6+42 = 49 Patterns) = (1+6)^2 = 7^2

#### Source Family (1+6 = 7 patterns)
Source × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}

1-1. **Void** - Source Aspect of Source

2-01. **Energy** - Fundamental animating force (Source × Polarity)
2-02. **Pattern** - Repeated types of order (Source × Structure)
2-03. **Power** - Productivity per unit time (Source × Exchange)
2-04. **Transformity** - Qualitative complexification (Source × Creativity)
2-05. **Resource** - Transformation of matter/energy (Source × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm})
2-06. **Autopoiesis** - Ongoing self-authoring (Source × Dynamics)

#### Dynamics Family (6 patterns)

7. **System** - Ordered activity of synergistic whole
8. **Spontaneity** - Coordinated impromptu reaction
9. **Feedback** - Dynamic adjustments through causal loops
10. **Synergy** - Benefit through combination
11. **Agency-Communion** - Tension between integration/disintegration
12. **Iterate** - Repeated cycles of incremental change

#### Creativity Family (6 patterns)

13. **Evolution** - Leap to higher complexity
14. **Emergence** - Moment of creative development
15. **Growth** - Developmental increase
16. **Adaptation** - Structural alterations
17. **Bifurcation** - Point of state change
18. **Seed** - Repeated emergence of beginnings

#### Exchange Family (6 patterns)

19. **Process** - Linear stage-by-stage development
20. **Uniqueness** - Difference between elements
21. **Trade** - Simple reciprocation
22. **Capture** - Structure to obtain yield
23. **Balance** - Dynamic equilibrium
24. **Cycle** - Circuit of phases

#### Structure Family (6 patterns)

25. **Holarchy** - Nested systems within systems
26. **Complexity** - Number of elements and connections
27. **Network** - Inter-connective architecture
28. **Hierarchy** - Ranking of levels
29. **Holon** - Part/whole duality
30. **Boundary** - Design of limiting edge

#### Polarity Family (6 patterns)

31. **Competition-Cooperation** - Fundamental systems-level duality
32. **Order-Chaos** - Oppositional creative dynamics
33. **Flows-Stores** - Dualistic form of resources
34. **Input-Output** - Systemic structuring dualism
35. **Concentration-Diffusion** - Foundational duality
36. **Expand-Contract** - Fundamental rhythmic duality

#### Rhythm Family (6 patterns)

37. **Enantiodromia** - Force of extreme movements
38. **Synchronization** - Creative inter-meshing in time
39. **Pulse** - Rhythmic surges of resource flows
40. **Cadence** - Structuring of rhythms
41. **Swing** - Movement between poles
42. **Repetition** - Simple ongoing recurrence

---

**Total: 1 + 6 + 42 = 49 Patterns**

## Pattern Matrix

|             | Dynamics | Creativity | Exchange | Structure | Polarity | Rhythm |
|-------------|----------|------------|----------|-----------|----------|--------|
| **Source**  | Energy   | Pattern    | Power    | Transformity | Resource | Autopoiesis |
| **Dynamics** | System   | Spontaneity | Feedback | Synergy   | Agency-Communion | Iterate |
| **Creativity** | Evolution | Emergence | Growth | Adaptation | Bifurcation | Seed |
| **Exchange** | Process | Uniqueness | Trade | Capture | Balance | Cycle |
| **Structure** | Holarchy | Complexity | Network | Hierarchy | Holon | Boundary |
| **Polarity** | Competition-Cooperation | Order-Chaos | Flows-Stores | Input-Output | Concentration-Diffusion | Expand-Contract |
| **Rhythm** | Enantiodromia | Synchronization | Pulse | Cadence | Swing | Repetition |

## Quick Access by Domain

### Temporality Patterns
- Rhythm (1st order)
- Repetition, Swing, Cadence, Pulse, Synchronization, Enantiodromia (2nd order)
- Autopoiesis, Iterate, Seed, Cycle (cross-domain)

### Spatiality Patterns
- Structure (1st order)
- Boundary, Holon, Hierarchy, Network, Complexity, Holarchy (2nd order)
- Transformity, Capture (cross-domain)

### Relationality Patterns
- Exchange (1st order)
- Cycle, Balance, Capture, Trade, Uniqueness, Process (2nd order)
- Power, Feedback, Growth, Network (cross-domain)

### Differentiation Patterns
- Polarity (1st order)
- Expand-Contract, Concentration-Diffusion, Input-Output, Flows-Stores, Order-Chaos, Competition-Cooperation (2nd order)
- Resource, Agency-Communion, Bifurcation, Balance, Holon, Swing (cross-domain)

### Generativity Patterns
- Creativity (1st order)
- Seed, Bifurcation, Adaptation, Growth, Emergence, Evolution (2nd order)
- Pattern, Spontaneity, Uniqueness (cross-domain)

### Processuality Patterns
- Dynamics (1st order)
- Iterate, Agency-Communion, Synergy, Feedback, Spontaneity, System (2nd order)
- Energy, Evolution, Process, Holarchy (cross-domain)

## Code Examples

### Scheme

```scheme
;; Load the system
(load "scheme/pattern-dynamics-49.scm")

;; Get a specific pattern
(define rhythm-pattern (car (get-patterns-by-major-aspect 'rhythm)))

;; List all 49 patterns
(display-49-pattern-holarchy)

;; Get just first-order
(define first-order (get-patterns-by-order 1))
(length first-order) ; => 6
```

### TypeScript

```typescript
import {
  patternHolarchy,
  getAllPatterns,
  getPatternsByMajorAspect,
  findPatternByName
} from './lib/pattern-dynamics-49.data';

// Get all patterns
const all = getAllPatterns(); // 49 patterns

// Get specific family
const rhythmFamily = getPatternsByMajorAspect('rhythm'); // 6 patterns

// Find specific pattern
const emergence = findPatternByName('emergence');
console.log(emergence?.description);

// Access pattern details
const pattern = rhythmFamily[0];
console.log({
  name: pattern.name,
  major: pattern.majorAspect,
  minor: pattern.minorAspect,
  role: pattern.role,
  principle: pattern.principle
});
```

## Pattern Relationships

### Generates Relationships
- Source generates → 6 First-order patterns
- Each First-order generates → 6 Second-order patterns

### Crosses-With Relationships
- Major aspect × Minor aspect → Second-order pattern
- Example: Rhythm × Creativity → Synchronization

### Emerges-From Relationships
- All patterns emerge from NonDual Origin
- Source as direct manifestation
- First/Second-order as differentiated emergence

### Returns-To Relationships
- All patterns return to NonDual Origin
- Through Empirical Domain enactment
- Cyclical evolution process

## Integral Semiotic Structure

Every pattern has:
- **Firstness**: Sign / Quality / 1st-person perspective
- **Secondness**: Object / Actuality / 3rd-person perspective
- **Thirdness**: Interpretant / Law / 2nd-person perspective

## Files Reference

- **Scheme**: `scheme/pattern-dynamics-49.scm`
- **Tests**: `scheme/test-pattern-dynamics-49.scm`
- **Types**: `src/lib/pattern-dynamics-49.types.ts`
- **Data**: `src/lib/pattern-dynamics-49.data.ts`
- **Docs**: `PATTERN-DYNAMICS-49.md`

## Additional Resources

- **PD49Table.md** - Original Pattern Dynamics table
- **integral-semiotic-enactment.md** - ISR framework
- **PATTERN-ANALYSIS.md** - Pattern Dynamics analysis

---

**Quick Stats**
- Total Patterns: 49
- Zeroth-Order: 1 (Source)
- First-Order: 6
- Second-Order: 42 (7 families × 6 patterns)
- Lines of Code: 2,244
- Tests: 10 (all passing)
- Security: 0 vulnerabilities


### Third-Order (294 Patterns)

#### Source Family (6 patterns)
Source × {Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm} × {Source, Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm}
Source x {Energy, Pattern, Power, Transformity, Resource, Autopoiesis, System, Spontaneity, Feedback, Synergy, Agency-Communion, Iterate, Evolution, Emergence, Growth, Adaptation, Bifurcation, Seed, Process, Uniqueness, Trade, Capture, Balance, Cycle, Holarchy, Complexity, Network, Hierarchy, Holon, Boundary, Competition-Cooperation, Order-Chaos, Flows-Stores, Input-Output, Concentration-Diffusion, Expand-Contract, Enantiodromia, Synchronization, Pulse, Cadence, Swing, Repetition}
1. **Energy** - Fundamental animating force
2. **Pattern** - Repeated types of order
3. **Power** - Productivity per unit time
4. **Transformity** - Qualitative complexification
5. **Resource** - Transformation of matter/energy
6. **Autopoiesis** - Ongoing self-authoring

#### Dynamics Family (6 patterns)

7. **System** - Ordered activity of synergistic whole
8. **Spontaneity** - Coordinated impromptu reaction
9. **Feedback** - Dynamic adjustments through causal loops
10. **Synergy** - Benefit through combination
11. **Agency-Communion** - Tension between integration/disintegration
12. **Iterate** - Repeated cycles of incremental change

#### Creativity Family (6 patterns)

13. **Evolution** - Leap to higher complexity
14. **Emergence** - Moment of creative development
15. **Growth** - Developmental increase
16. **Adaptation** - Structural alterations
17. **Bifurcation** - Point of state change
18. **Seed** - Repeated emergence of beginnings

#### Exchange Family (6 patterns)

19. **Process** - Linear stage-by-stage development
20. **Uniqueness** - Difference between elements
21. **Trade** - Simple reciprocation
22. **Capture** - Structure to obtain yield
23. **Balance** - Dynamic equilibrium
24. **Cycle** - Circuit of phases

#### Structure Family (6 patterns)

25. **Holarchy** - Nested systems within systems
26. **Complexity** - Number of elements and connections
27. **Network** - Inter-connective architecture
28. **Hierarchy** - Ranking of levels
29. **Holon** - Part/whole duality
30. **Boundary** - Design of limiting edge

#### Polarity Family (6 patterns)

31. **Competition-Cooperation** - Fundamental systems-level duality
32. **Order-Chaos** - Oppositional creative dynamics
33. **Flows-Stores** - Dualistic form of resources
34. **Input-Output** - Systemic structuring dualism
35. **Concentration-Diffusion** - Foundational duality
36. **Expand-Contract** - Fundamental rhythmic duality

#### Rhythm Family (6 patterns)

37. **Enantiodromia** - Force of extreme movements
38. **Synchronization** - Creative inter-meshing in time
39. **Pulse** - Rhythmic surges of resource flows
40. **Cadence** - Structuring of rhythms
41. **Swing** - Movement between poles
42. **Repetition** - Simple ongoing recurrence

---

**Total: 1 + 6 + 42 = 49 Patterns**

## Pattern Matrix

|             | Dynamics | Creativity | Exchange | Structure | Polarity | Rhythm |
|-------------|----------|------------|----------|-----------|----------|--------|
| **Source**  | Energy   | Pattern    | Power    | Transformity | Resource | Autopoiesis |
| **Dynamics** | System   | Spontaneity | Feedback | Synergy   | Agency-Communion | Iterate |
| **Creativity** | Evolution | Emergence | Growth | Adaptation | Bifurcation | Seed |
| **Exchange** | Process | Uniqueness | Trade | Capture | Balance | Cycle |
| **Structure** | Holarchy | Complexity | Network | Hierarchy | Holon | Boundary |
| **Polarity** | Competition-Cooperation | Order-Chaos | Flows-Stores | Input-Output | Concentration-Diffusion | Expand-Contract |
| **Rhythm** | Enantiodromia | Synchronization | Pulse | Cadence | Swing | Repetition |

## Quick Access by Domain

### Temporality Patterns
- Rhythm (1st order)
- Repetition, Swing, Cadence, Pulse, Synchronization, Enantiodromia (2nd order)
- Autopoiesis, Iterate, Seed, Cycle (cross-domain)

### Spatiality Patterns
- Structure (1st order)
- Boundary, Holon, Hierarchy, Network, Complexity, Holarchy (2nd order)
- Transformity, Capture (cross-domain)

### Relationality Patterns
- Exchange (1st order)
- Cycle, Balance, Capture, Trade, Uniqueness, Process (2nd order)
- Power, Feedback, Growth, Network (cross-domain)

### Differentiation Patterns
- Polarity (1st order)
- Expand-Contract, Concentration-Diffusion, Input-Output, Flows-Stores, Order-Chaos, Competition-Cooperation (2nd order)
- Resource, Agency-Communion, Bifurcation, Balance, Holon, Swing (cross-domain)

### Generativity Patterns
- Creativity (1st order)
- Seed, Bifurcation, Adaptation, Growth, Emergence, Evolution (2nd order)
- Pattern, Spontaneity, Uniqueness (cross-domain)

### Processuality Patterns
- Dynamics (1st order)
- Iterate, Agency-Communion, Synergy, Feedback, Spontaneity, System (2nd order)
- Energy, Evolution, Process, Holarchy (cross-domain)

## Code Examples

### Scheme

```scheme
;; Load the system
(load "scheme/pattern-dynamics-49.scm")

;; Get a specific pattern
(define rhythm-pattern (car (get-patterns-by-major-aspect 'rhythm)))

;; List all 49 patterns
(display-49-pattern-holarchy)

;; Get just first-order
(define first-order (get-patterns-by-order 1))
(length first-order) ; => 6
```

### TypeScript

```typescript
import {
  patternHolarchy,
  getAllPatterns,
  getPatternsByMajorAspect,
  findPatternByName
} from './lib/pattern-dynamics-49.data';

// Get all patterns
const all = getAllPatterns(); // 49 patterns

// Get specific family
const rhythmFamily = getPatternsByMajorAspect('rhythm'); // 6 patterns

// Find specific pattern
const emergence = findPatternByName('emergence');
console.log(emergence?.description);

// Access pattern details
const pattern = rhythmFamily[0];
console.log({
  name: pattern.name,
  major: pattern.majorAspect,
  minor: pattern.minorAspect,
  role: pattern.role,
  principle: pattern.principle
});
```

## Pattern Relationships

### Generates Relationships
- Source generates → 6 First-order patterns
- Each First-order generates → 6 Second-order patterns

### Crosses-With Relationships
- Major aspect × Minor aspect → Second-order pattern
- Example: Rhythm × Creativity → Synchronization

### Emerges-From Relationships
- All patterns emerge from NonDual Origin
- Source as direct manifestation
- First/Second-order as differentiated emergence

### Returns-To Relationships
- All patterns return to NonDual Origin
- Through Empirical Domain enactment
- Cyclical evolution process

## Integral Semiotic Structure

Every pattern has:
- **Firstness**: Sign / Quality / 1st-person perspective
- **Secondness**: Object / Actuality / 3rd-person perspective
- **Thirdness**: Interpretant / Law / 2nd-person perspective

## Files Reference

- **Scheme**: `scheme/pattern-dynamics-49.scm`
- **Tests**: `scheme/test-pattern-dynamics-49.scm`
- **Types**: `src/lib/pattern-dynamics-49.types.ts`
- **Data**: `src/lib/pattern-dynamics-49.data.ts`
- **Docs**: `PATTERN-DYNAMICS-49.md`

## Additional Resources

- **PD49Table.md** - Original Pattern Dynamics table
- **integral-semiotic-enactment.md** - ISR framework
- **PATTERN-ANALYSIS.md** - Pattern Dynamics analysis

---

**Quick Stats**
- Total Patterns: 49
- Zeroth-Order: 1 (Source)
- First-Order: 6
- Second-Order: 42 (7 families × 6 patterns)
- Lines of Code: 2,244
- Tests: 10 (all passing)
- Security: 0 vulnerabilities
