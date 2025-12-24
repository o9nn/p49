# Pattern Dynamics 49 Patterns - AIML Implementation

This directory contains AIML (Artificial Intelligence Markup Language) files implementing all 49 Pattern Dynamics patterns for chatbot integration.

## 📚 Overview

AIML is an XML-based language used to create natural language chatbot responses. This implementation provides comprehensive pattern-matching rules and responses for all 49 Pattern Dynamics patterns, enabling conversational interfaces to explain and discuss the patterns.

## 🗂️ File Structure

### Master File
- **`patterns-49-master.aiml`** - Main entry point that loads all pattern files and provides high-level Pattern Dynamics information

### Pattern Files (10 files total)

1. **`00-source-pattern.aiml`** - Source pattern (zeroth-order) with 7 aspects
2. **`01-first-order-patterns.aiml`** - 6 first-order patterns (Rhythm, Polarity, Structure, Exchange, Creativity, Dynamics)
3. **`02-source-family.aiml`** - Source family (6 second-order patterns)
4. **`03-dynamics-family.aiml`** - Dynamics family (6 second-order patterns)
5. **`04-creativity-family.aiml`** - Creativity family (6 second-order patterns)
6. **`05-exchange-family.aiml`** - Exchange family (6 second-order patterns)
7. **`06-structure-family.aiml`** - Structure family (6 second-order patterns)
8. **`07-polarity-family.aiml`** - Polarity family (6 second-order patterns)
9. **`08-rhythm-family.aiml`** - Rhythm family (6 second-order patterns)

## 🎯 Pattern Coverage

### Complete 49 Pattern Implementation

- **1 Zeroth-Order Pattern**: Source (the foundational ground)
- **6 First-Order Patterns**: Rhythm, Polarity, Structure, Exchange, Creativity, Dynamics
- **42 Second-Order Patterns**: 7 families × 6 patterns each

### Holarchical Structure

```
Source (0)
├── First-Order Patterns (6)
│   ├── Rhythm (temporality)
│   ├── Polarity (differentiation)
│   ├── Structure (spatiality)
│   ├── Exchange (relationality)
│   ├── Creativity (generativity)
│   └── Dynamics (processuality)
└── Second-Order Patterns (42)
    ├── Source Family (6)
    ├── Dynamics Family (6)
    ├── Creativity Family (6)
    ├── Exchange Family (6)
    ├── Structure Family (6)
    ├── Polarity Family (6)
    └── Rhythm Family (6)
```

## 💬 Query Patterns

Each pattern supports multiple query patterns:

### Basic Information Queries
- `WHAT IS [pattern-name]` - Get basic description
- `TELL ME ABOUT [pattern-name]` - Get description
- `EXPLAIN [pattern-name]` - Get detailed explanation
- `DESCRIBE [pattern-name]` - Get description only

### Detailed Queries
- `WHAT IS THE PRINCIPLE OF [pattern-name]` - Get the principle
- `[pattern-name] PRINCIPLE` - Get the principle formatted
- `WHAT IS THE ROLE OF [pattern-name]` - Get the role (for second-order patterns)
- `WHAT IS THE DOMAIN OF [pattern-name]` - Get the domain (for first-order patterns)
- `WHAT ORDER IS [pattern-name]` - Get pattern order classification
- `WHAT FAMILY IS [pattern-name]` - Get family information (for second-order patterns)

### Meta Queries (Master File)
- `WHAT IS PATTERN DYNAMICS` - Overview of the framework
- `HOW MANY PATTERNS ARE THERE` - Pattern count breakdown
- `LIST ALL PATTERNS` - Complete pattern listing
- `WHAT ARE THE FIRST ORDER PATTERNS` - List of 6 first-order patterns
- `WHAT IS INTEGRAL SEMIOTIC REALISM` - Theoretical framework explanation

## 🚀 Usage

### With Python AIML Library

```python
import aiml

# Create kernel
kernel = aiml.Kernel()

# Load master file (which loads all pattern files)
kernel.learn("aiml/patterns-49-master.aiml")

# Query patterns
response = kernel.respond("WHAT IS RHYTHM")
print(response)  # "Rhythm is Temporal patterns, cycles, and repetition in time and space"

response = kernel.respond("WHAT IS THE PRINCIPLE OF SYNERGY")
print(response)  # "Dynamical structures: balance synergistic integration with component integrity"
```

### With Other AIML Interpreters

Most AIML interpreters support the standard AIML 2.0 format. Simply:

1. Load the `patterns-49-master.aiml` file
2. The `<learn>` tags will automatically load all pattern files
3. Start querying patterns

### Direct File Loading

If your interpreter doesn't support `<learn>` tags, load files individually:

```python
# Load all files in order
kernel.learn("aiml/00-source-pattern.aiml")
kernel.learn("aiml/01-first-order-patterns.aiml")
kernel.learn("aiml/02-source-family.aiml")
# ... continue for all files
```

## 📋 Example Conversations

### Example 1: First-Order Pattern
```
User: WHAT IS DYNAMICS
Bot: Dynamics is Dynamic processes, feedback loops, and systemic integration

User: WHAT IS THE PRINCIPLE OF DYNAMICS
Bot: Process integration through feedback

User: WHAT IS THE DOMAIN OF DYNAMICS
Bot: The domain of dynamics is processuality
```

### Example 2: Second-Order Pattern
```
User: WHAT IS FEEDBACK
Bot: Feedback is Dynamic adjustments through causal loops - self-regulation

User: WHAT IS THE ROLE OF FEEDBACK
Bot: Enable self-correcting behavior

User: WHAT FAMILY IS FEEDBACK
Bot: feedback is part of the dynamics family, created from dynamics × exchange

User: WHAT IS THE PRINCIPLE OF FEEDBACK
Bot: Adjustment dynamics: balance feedback sensitivity with stability
```

### Example 3: Meta Information
```
User: HOW MANY PATTERNS ARE THERE
Bot: There are 49 patterns in total: 1 zeroth-order pattern (Source), 6 first-order patterns, and 42 second-order patterns organized into 7 families.

User: WHAT IS PATTERN DYNAMICS
Bot: Pattern Dynamics is a comprehensive framework for understanding complex adaptive systems through 49 archetypal patterns...
```

## 🔧 Generation

The AIML files are generated from the TypeScript pattern data using:

```bash
npx tsx scripts/generate-aiml.ts
```

This ensures consistency with the main Pattern Dynamics implementation in TypeScript and Scheme.

## 📖 Pattern Details

### First-Order Patterns

| Pattern | Domain | Principle |
|---------|--------|-----------|
| **Rhythm** | Temporality | Temporal organization through recurrence |
| **Polarity** | Differentiation | Creative tension through complementary opposition |
| **Structure** | Spatiality | Spatial organization through form |
| **Exchange** | Relationality | Relational coordination through reciprocity |
| **Creativity** | Generativity | Developmental transformation through novelty |
| **Dynamics** | Processuality | Process integration through feedback |

### Second-Order Pattern Families

Each family contains 6 patterns formed by crossing the major aspect with each of the 6 first-order patterns:

- **Source Family**: Energy, Pattern, Power, Transformity, Resource, Autopoiesis
- **Dynamics Family**: System, Spontaneity, Feedback, Synergy, Agency-Communion, Iterate
- **Creativity Family**: Evolution, Emergence, Growth, Adaptation, Bifurcation, Seed
- **Exchange Family**: Process, Uniqueness, Trade, Capture, Balance, Cycle
- **Structure Family**: Holarchy, Complexity, Network, Hierarchy, Holon, Boundary
- **Polarity Family**: Competition-Cooperation, Order-Chaos, Flows-Stores, Input-Output, Concentration-Diffusion, Expand-Contract
- **Rhythm Family**: Enantiodromia, Synchronization, Pulse, Cadence, Swing, Repetition

## 🏗️ Integration with Integral Semiotic Realism

All patterns are grounded in the Integral Semiotic Realism (ISR) framework, which integrates:

- **Peircean Semiotics**: Sign-Object-Interpretant triadic structure
- **Critical Realism**: Stratified ontology (Actual, Intransitive, Empirical domains)
- **Integral Theory**: Multi-perspectival AQAL framework (1st, 2nd, 3rd person)
- **Nondual Philosophy**: NonDual Origin as undifferentiated source

## 📝 Customization

To add custom query patterns:

1. Edit the relevant AIML file
2. Add new `<category>` blocks with your pattern and template:

```xml
<category>
  <pattern>YOUR CUSTOM PATTERN</pattern>
  <template>Your custom response</template>
</category>
```

Or regenerate from the source by modifying `scripts/generate-aiml.ts` and running:

```bash
npx tsx scripts/generate-aiml.ts
```

## 🔍 Validation

The AIML files follow AIML 2.0 specification and can be validated using standard XML validators or AIML-specific validators.

Basic XML validation:
```bash
xmllint --noout aiml/*.aiml
```

## 📚 Related Documentation

- [Main README](../README.md) - Project overview
- [Pattern Dynamics 49 Documentation](../PATTERN-DYNAMICS-49.md) - Complete pattern details
- [Integral Semiotic Enactment](../integral-semiotic-enactment.md) - Theoretical framework
- [Pattern Materials](../patterns/) - Source materials and diagrams

## 🤖 Supported AIML Interpreters

These files work with:
- Python AIML library (`python-aiml`)
- Pandorabots
- Program-O
- RiveScript (with AIML compatibility)
- Most AIML 2.0 compliant interpreters

## 📄 License

Consistent with the main repository license.

---

**Generated**: 2024-12-24  
**Total Patterns**: 49 (1 + 6 + 42)  
**Total Files**: 10 AIML files  
**Format**: AIML 2.0 (XML)
