# Pattern Dynamics - Integral Semiotic Realism

An interactive exploration of the **Pattern Primary Defining Archetype** through both visual and computational representations.

## Overview

This project implements the Integral Semiotic Realism framework, which models the dynamic relationships between:
- **Firstness** (Sign/Epistemology/1st Person Perspective)
- **Secondness** (Object/Ontology/3rd Person Perspective)  
- **Thirdness** (Interpretant/Methodology/2nd Person Perspective)

The framework integrates three domains (Actual, Intransitive, Empirical) through four fundamental processes (continuous signification, epistemic emergence, ontological emergence, and nondual return).

## Project Components

### 🎨 Web Visualization (TypeScript/React)
An interactive web application featuring:
- SVG-based infinity loop diagram
- Interactive node exploration
- Domain and perspective displays
- Responsive design for desktop and mobile

**Run the visualization:**
```bash
npm install
npm run dev
```

### 🧮 Scheme Implementation (Pure Functional)
A complete computational model in pure Scheme expressions:
- Core data structures for nodes, processes, domains, and perspectives
- Cycle traversal and transformation functions
- Pattern evolution and cosmic habit formation
- Comprehensive test suite (67 tests, all passing)

**Explore the Scheme model:**
```bash
cd scheme
guile demo.scm        # Run demonstration
guile utilities.scm   # Advanced utilities
guile tests.scm       # Run test suite
```

See [scheme/README.md](scheme/README.md) for detailed documentation.

## Philosophical Foundation

Based on:
1. **Peircean Semiotics** - The triadic sign relation
2. **Critical Realism** - Stratified ontology  
3. **Integral Theory** - Multi-perspectival approach
4. **NonDual Philosophy** - Recognition of the undifferentiated source

## Documentation

- [PRD.md](PRD.md) - Product requirements and design specifications
- [scheme/README.md](scheme/README.md) - Scheme implementation guide
- Referenced documents:
  - Integral Semiotic Enactment - Key Discovery
  - Pattern Dynamics Rosetta Stone - Implementation Plan
  - Pattern Rosetta Stone - Complete Theoretical Framework

## Development

### Web Application
```bash
npm install          # Install dependencies
npm run dev         # Start development server
npm run build       # Build for production
npm run lint        # Run linter
```

### Scheme Implementation
Requires Guile Scheme 3.0 or compatible Scheme interpreter:
```bash
# Install Guile (Ubuntu/Debian)
sudo apt-get install guile-3.0

# Run demonstrations
cd scheme
guile demo.scm

# Interactive REPL
guile -l pattern-primary-archetype.scm
```

## License

MIT License - See [LICENSE](LICENSE) file for details.
