/**
 * Generate AIML files for all 49 Pattern Dynamics patterns
 * 
 * This script reads the pattern data and generates AIML XML files
 * that can be used for chatbot pattern recognition and responses.
 */

import * as fs from 'fs';
import * as path from 'path';

// Source pattern with 7 aspects
const sourcePattern = {
  name: 'source',
  order: 0,
  description: 'The foundational ground from which all patterns emerge',
  type: 'zeroth-order-pattern',
  aspects: {
    sourceSource: { name: 'void', altName: 'ground-source', description: 'The foundational ground of universal matter/energy consciousness', role: 'Signify consciousness necessary for order', principle: 'Conscious order' },
    sourceDynamics: { name: 'energy', altName: 'ground-transformations', description: 'Fundamental animating force of reality', role: 'Drive dynamic change and transformation', principle: 'Dynamic transformation' },
    sourceCreativity: { name: 'pattern', altName: 'dynamic-order', description: 'Repeated types of order', role: 'Provide successful templates of systemic design', principle: 'Potential and restrictiveness of form' },
    sourceExchange: { name: 'power', altName: 'work-rate', description: 'Productivity per unit time', role: 'Provide systemic productivity', principle: 'Productivity rates' },
    sourceStructure: { name: 'transformity', altName: 'order-accrual', description: 'Qualitative complexification', role: 'Bring advantages of structural increases in order', principle: 'Complexity creation' },
    sourcePolarity: { name: 'resource', altName: 'source-distinctions', description: 'Process of transformation of matter/energy', role: 'Provide different qualities of matter/energy', principle: 'Source transformations' },
    sourceRhythm: { name: 'autopoiesis', altName: 'self-creation', description: 'Ongoing self-authoring', role: 'Maintain unique system through creative growth', principle: 'Creative existence' },
  }
};

// First-order patterns (6 patterns)
const firstOrderPatterns = [
  { name: 'rhythm', order: 1, description: 'Temporal patterns, cycles, and repetition in time and space', domain: 'temporality', principle: 'Temporal organization through recurrence', type: 'first-order-pattern' },
  { name: 'polarity', order: 1, description: 'Fundamental dualities and complementary oppositions', domain: 'differentiation', principle: 'Creative tension through complementary opposition', type: 'first-order-pattern' },
  { name: 'structure', order: 1, description: 'Organizational forms, boundaries, and spatial arrangements', domain: 'spatiality', principle: 'Spatial organization through form', type: 'first-order-pattern' },
  { name: 'exchange', order: 1, description: 'Flows, trades, and reciprocal exchanges between elements', domain: 'relationality', principle: 'Relational coordination through reciprocity', type: 'first-order-pattern' },
  { name: 'creativity', order: 1, description: 'Emergence of novelty, innovation, and creative transformation', domain: 'generativity', principle: 'Developmental transformation through novelty', type: 'first-order-pattern' },
  { name: 'dynamics', order: 1, description: 'Dynamic processes, feedback loops, and systemic integration', domain: 'processuality', principle: 'Process integration through feedback', type: 'first-order-pattern' },
];

// Second-order patterns (42 patterns organized by families)
const secondOrderPatterns = [
  // Source Family (6)
  { name: 'energy', altName: 'ground-transformations', major: 'source', minor: 'dynamics', description: 'The fundamental animating force of reality - rhythmic vibration in space/time', role: 'Drive dynamic change and transformation processes', principle: 'Dynamic transformation: balance energy use with availability', type: 'second-order-pattern', order: 2 },
  { name: 'pattern', altName: 'dynamic-order', major: 'source', minor: 'creativity', description: 'Repeated types of order - recurrent design arrangements', role: 'Provide successful templates of systemic design', principle: 'Potential and restrictiveness of form: balance patterned forms with unrestricted states', type: 'second-order-pattern', order: 2 },
  { name: 'power', altName: 'work-rate', major: 'source', minor: 'exchange', description: 'Productivity per unit time - rate and efficiency of systemic functioning', role: 'Provide systemic productivity', principle: 'Productivity rates: balance work rate with resource flow', type: 'second-order-pattern', order: 2 },
  { name: 'transformity', altName: 'order-accrual', major: 'source', minor: 'structure', description: 'Qualitative complexification - transformation to higher complexity', role: 'Bring advantages of structural increases in order', principle: 'Complexity creation: balance transformity with available energy', type: 'second-order-pattern', order: 2 },
  { name: 'resource', altName: 'source-distinctions', major: 'source', minor: 'polarity', description: 'Process of transformation of matter/energy - conservation through transformation', role: 'Provide different qualities of matter/energy for unique purposes', principle: 'Source transformations: balance number of transformations with usefulness', type: 'second-order-pattern', order: 2 },
  { name: 'autopoiesis', altName: 'self-creation', major: 'source', minor: 'rhythm', description: 'Ongoing self-authoring - maintenance and creative unfolding', role: 'Maintain unique system of order through creative growth', principle: 'Creative existence: balance self-development with self-maintenance', type: 'second-order-pattern', order: 2 },
  
  // Dynamics Family (6)
  { name: 'system', altName: 'integrated-dynamics', major: 'dynamics', minor: 'dynamics', description: 'Ordered activity of synergistic whole - integrated functioning', role: 'Enable coordinated whole-system functioning', principle: 'Integrated dynamics: balance system coherence with subsystem autonomy', type: 'second-order-pattern', order: 2 },
  { name: 'spontaneity', altName: 'dynamic-creativity', major: 'dynamics', minor: 'creativity', description: 'Coordinated impromptu reaction - dynamic responsiveness', role: 'Enable adaptive responses to novelty', principle: 'Dynamic creativity: balance spontaneous response with deliberate planning', type: 'second-order-pattern', order: 2 },
  { name: 'feedback', altName: 'adjustment-dynamics', major: 'dynamics', minor: 'exchange', description: 'Dynamic adjustments through causal loops - self-regulation', role: 'Enable self-correcting behavior', principle: 'Adjustment dynamics: balance feedback sensitivity with stability', type: 'second-order-pattern', order: 2 },
  { name: 'synergy', altName: 'dynamical-structures', major: 'dynamics', minor: 'structure', description: 'Benefit through combination - emergent whole greater than parts', role: 'Enable cooperative advantage', principle: 'Dynamical structures: balance synergistic integration with component integrity', type: 'second-order-pattern', order: 2 },
  { name: 'agency-communion', altName: 'dynamic-polarity', major: 'dynamics', minor: 'polarity', description: 'Tension between integration and disintegration - push-pull dynamics', role: 'Balance wholeness-seeking and individuality-seeking forces', principle: 'Dynamic polarity: balance agency drives with communion drives', type: 'second-order-pattern', order: 2 },
  { name: 'iterate', altName: 'dynamic-rhythm', major: 'dynamics', minor: 'rhythm', description: 'Repeated cycles of incremental change - iterative development', role: 'Enable step-by-step refinement', principle: 'Dynamic rhythm: balance iteration frequency with iteration magnitude', type: 'second-order-pattern', order: 2 },
  
  // Creativity Family (6)
  { name: 'evolution', altName: 'creative-dynamics', major: 'creativity', minor: 'dynamics', description: 'Leap to higher complexity - transformative development', role: 'Enable developmental progression', principle: 'Creative dynamics: balance evolutionary change with continuity', type: 'second-order-pattern', order: 2 },
  { name: 'emergence', altName: 'innovative-arising', major: 'creativity', minor: 'creativity', description: 'Moment of creative development - novel appearance', role: 'Enable new forms to arise', principle: 'Innovative arising: balance emergent novelty with systemic coherence', type: 'second-order-pattern', order: 2 },
  { name: 'growth', altName: 'creative-prosperity', major: 'creativity', minor: 'exchange', description: 'Developmental increase - expansive flourishing', role: 'Enable increasing capacity and capability', principle: 'Creative prosperity: balance growth rate with resource availability', type: 'second-order-pattern', order: 2 },
  { name: 'adaptation', altName: 'structural-adjustments', major: 'creativity', minor: 'structure', description: 'Structural alterations - responsive modification', role: 'Enable contextual fit', principle: 'Structural adjustments: balance adaptive change with stable identity', type: 'second-order-pattern', order: 2 },
  { name: 'bifurcation', altName: 'liminal-creation', major: 'creativity', minor: 'polarity', description: 'Point of state change - threshold crossing', role: 'Enable transformative transitions', principle: 'Liminal creation: balance stability duration with transformation readiness', type: 'second-order-pattern', order: 2 },
  { name: 'seed', altName: 'emergent-creation', major: 'creativity', minor: 'rhythm', description: 'Repeated emergence of beginnings - generative initiation', role: 'Enable recurring new starts', principle: 'Emergent creation: balance seeding frequency with maturation time', type: 'second-order-pattern', order: 2 },
  
  // Exchange Family (6)
  { name: 'process', altName: 'operation-dynamics', major: 'exchange', minor: 'dynamics', description: 'Linear stage-by-stage development - sequential unfolding', role: 'Enable ordered progression', principle: 'Operation dynamics: balance process efficiency with process thoroughness', type: 'second-order-pattern', order: 2 },
  { name: 'uniqueness', altName: 'exchange-creation', major: 'exchange', minor: 'creativity', description: 'Difference between elements - distinctive character', role: 'Enable specialized contributions', principle: 'Exchange creation: balance uniqueness with commonality', type: 'second-order-pattern', order: 2 },
  { name: 'trade', altName: 'essence-of-exchange', major: 'exchange', minor: 'exchange', description: 'Simple reciprocation - mutual exchange', role: 'Enable fair transactions', principle: 'Essence of exchange: balance giving with receiving', type: 'second-order-pattern', order: 2 },
  { name: 'capture', altName: 'flow-container', major: 'exchange', minor: 'structure', description: 'Structure to obtain yield - harvesting mechanism', role: 'Enable resource acquisition', principle: 'Flow container: balance capture efficiency with flow continuity', type: 'second-order-pattern', order: 2 },
  { name: 'balance', altName: 'relational-duality', major: 'exchange', minor: 'polarity', description: 'Dynamic equilibrium - stable interplay', role: 'Enable sustainable relationships', principle: 'Relational duality: balance opposing forces for dynamic equilibrium', type: 'second-order-pattern', order: 2 },
  { name: 'cycle', altName: 'exchange-phases', major: 'exchange', minor: 'rhythm', description: 'Circuit of phases - recurring sequence', role: 'Enable sustainable circulation', principle: 'Exchange phases: balance cycle completion with cycle renewal', type: 'second-order-pattern', order: 2 },
  
  // Structure Family (6)
  { name: 'holarchy', altName: 'structural-dynamics', major: 'structure', minor: 'dynamics', description: 'Nested systems within systems - hierarchical wholeness', role: 'Enable multi-level organization', principle: 'Structural dynamics: balance nested hierarchy with distributed network', type: 'second-order-pattern', order: 2 },
  { name: 'complexity', altName: 'order-creation', major: 'structure', minor: 'creativity', description: 'Number of elements and connections - intricate elaboration', role: 'Enable sophisticated organization', principle: 'Order creation: balance complexity increase with manageable simplicity', type: 'second-order-pattern', order: 2 },
  { name: 'network', altName: 'relational-design', major: 'structure', minor: 'exchange', description: 'Inter-connective architecture - web of relationships', role: 'Enable distributed coordination', principle: 'Relational design: balance network connectivity with node autonomy', type: 'second-order-pattern', order: 2 },
  { name: 'hierarchy', altName: 'essential-structure', major: 'structure', minor: 'structure', description: 'Ranking of levels - ordered stratification', role: 'Enable vertical organization', principle: 'Essential structure: balance hierarchical order with horizontal collaboration', type: 'second-order-pattern', order: 2 },
  { name: 'holon', altName: 'part-whole', major: 'structure', minor: 'polarity', description: 'Part/whole duality - simultaneously part and whole', role: 'Enable nested wholeness', principle: 'Part-whole: balance part integrity with whole integration', type: 'second-order-pattern', order: 2 },
  { name: 'boundary', altName: 'edge-pattern', major: 'structure', minor: 'rhythm', description: 'Design of limiting edge - interface demarcation', role: 'Enable identity differentiation', principle: 'Edge pattern: balance boundary permeability with boundary integrity', type: 'second-order-pattern', order: 2 },
  
  // Polarity Family (6)
  { name: 'competition-cooperation', altName: 'part-whole-polarity', major: 'polarity', minor: 'dynamics', description: 'Fundamental systems-level duality - opposing collaborative modes', role: 'Balance competitive and cooperative forces', principle: 'Part-whole polarity: balance competition with cooperation', type: 'second-order-pattern', order: 2 },
  { name: 'order-chaos', altName: 'liminal-polarity', major: 'polarity', minor: 'creativity', description: 'Oppositional creative dynamics - edge of possibility', role: 'Balance structure with fluidity', principle: 'Liminal polarity: balance order maintenance with chaos exploration', type: 'second-order-pattern', order: 2 },
  { name: 'flows-stores', altName: 'relational-states', major: 'polarity', minor: 'exchange', description: 'Dualistic form of resources - movement and storage', role: 'Balance flow dynamics with storage capacity', principle: 'Relational states: balance resource flows with resource stores', type: 'second-order-pattern', order: 2 },
  { name: 'input-output', altName: 'polarity-structure', major: 'polarity', minor: 'structure', description: 'Systemic structuring dualism - interface dynamics', role: 'Balance intake with output', principle: 'Polarity structure: balance system inputs with system outputs', type: 'second-order-pattern', order: 2 },
  { name: 'concentration-diffusion', altName: 'primordial-duality', major: 'polarity', minor: 'polarity', description: 'Foundational duality - gathering and dispersing', role: 'Balance centralization with distribution', principle: 'Primordial duality: balance concentration with diffusion', type: 'second-order-pattern', order: 2 },
  { name: 'expand-contract', altName: 'rhythmic-duality', major: 'polarity', minor: 'rhythm', description: 'Fundamental rhythmic duality - opening and closing', role: 'Balance expansion with contraction', principle: 'Rhythmic duality: balance expansion phases with contraction phases', type: 'second-order-pattern', order: 2 },
  
  // Rhythm Family (6)
  { name: 'enantiodromia', altName: 'emergent-oppositions', major: 'rhythm', minor: 'dynamics', description: 'Force of extreme movements - running to opposite', role: 'Balance extreme swings', principle: 'Emergent oppositions: balance movement to extremes with return to center', type: 'second-order-pattern', order: 2 },
  { name: 'synchronization', altName: 'mesh-works', major: 'rhythm', minor: 'creativity', description: 'Creative inter-meshing in time - coordinated timing', role: 'Enable temporal coordination', principle: 'Mesh-works: balance synchronized coordination with independent timing', type: 'second-order-pattern', order: 2 },
  { name: 'pulse', altName: 'flow-surges', major: 'rhythm', minor: 'exchange', description: 'Rhythmic surges of resource flows - pulsing circulation', role: 'Enable pulsatile exchange', principle: 'Flow surges: balance pulse strength with pulse frequency', type: 'second-order-pattern', order: 2 },
  { name: 'cadence', altName: 'rhythm-structures', major: 'rhythm', minor: 'structure', description: 'Structuring of rhythms - ordered periodicity', role: 'Enable rhythmic organization', principle: 'Rhythm structures: balance rhythmic complexity with rhythmic simplicity', type: 'second-order-pattern', order: 2 },
  { name: 'swing', altName: 'iterative-extremes', major: 'rhythm', minor: 'polarity', description: 'Movement between poles - oscillating dynamics', role: 'Balance swings to extremes with adjustments to center', principle: 'Iterative extremes: balance swing amplitude with swing frequency', type: 'second-order-pattern', order: 2 },
  { name: 'repetition', altName: 'recurrent-order', major: 'rhythm', minor: 'rhythm', description: 'Simple ongoing recurrence - cyclic return', role: 'Enable pattern persistence', principle: 'Recurrent order: balance repetition with variation', type: 'second-order-pattern', order: 2 },
];

function escapeXml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&apos;');
}

function generateAimlCategory(pattern: string, template: string): string {
  return `  <category>
    <pattern>${escapeXml(pattern.toUpperCase())}</pattern>
    <template>${escapeXml(template)}</template>
  </category>`;
}

function generatePatternAiml(pattern: any): string {
  const categories: string[] = [];
  
  // Basic pattern query
  categories.push(generateAimlCategory(
    `WHAT IS ${pattern.name}`,
    `${pattern.name.charAt(0).toUpperCase() + pattern.name.slice(1)} is ${pattern.description}`
  ));
  
  categories.push(generateAimlCategory(
    `TELL ME ABOUT ${pattern.name}`,
    `${pattern.name.charAt(0).toUpperCase() + pattern.name.slice(1)} is ${pattern.description}`
  ));
  
  categories.push(generateAimlCategory(
    `EXPLAIN ${pattern.name}`,
    `${pattern.name.charAt(0).toUpperCase() + pattern.name.slice(1)}: ${pattern.description}`
  ));
  
  // Description query
  categories.push(generateAimlCategory(
    `DESCRIBE ${pattern.name}`,
    pattern.description
  ));
  
  // Principle query (if available)
  if (pattern.principle) {
    categories.push(generateAimlCategory(
      `WHAT IS THE PRINCIPLE OF ${pattern.name}`,
      pattern.principle
    ));
    
    categories.push(generateAimlCategory(
      `${pattern.name} PRINCIPLE`,
      `The principle of ${pattern.name} is: ${pattern.principle}`
    ));
  }
  
  // Role query (if available)
  if (pattern.role) {
    categories.push(generateAimlCategory(
      `WHAT IS THE ROLE OF ${pattern.name}`,
      pattern.role
    ));
  }
  
  // Domain query (if available)
  if (pattern.domain) {
    categories.push(generateAimlCategory(
      `WHAT IS THE DOMAIN OF ${pattern.name}`,
      `The domain of ${pattern.name} is ${pattern.domain}`
    ));
  }
  
  // Alternative name (if available)
  if (pattern.altName) {
    categories.push(generateAimlCategory(
      `WHAT IS ${pattern.altName.replace(/-/g, ' ')}`,
      `${pattern.altName} is another name for ${pattern.name}. ${pattern.description}`
    ));
  }
  
  // Order query
  categories.push(generateAimlCategory(
    `WHAT ORDER IS ${pattern.name}`,
    `${pattern.name} is a ${pattern.type === 'zeroth-order-pattern' ? 'zeroth' : pattern.order === 1 ? 'first' : 'second'}-order pattern`
  ));
  
  // For second-order patterns, add family info
  if (pattern.major && pattern.minor) {
    categories.push(generateAimlCategory(
      `WHAT FAMILY IS ${pattern.name}`,
      `${pattern.name} is part of the ${pattern.major} family, created from ${pattern.major} × ${pattern.minor}`
    ));
  }
  
  return categories.join('\n\n');
}

function generateAimlFile(patterns: any[], filename: string, title: string): void {
  const categories = patterns.map(p => generatePatternAiml(p)).join('\n\n');
  
  const content = `<?xml version="1.0" encoding="UTF-8"?>
<aiml version="2.0">
  <!-- ${title} -->
  <!-- Generated from Pattern Dynamics 49 implementation -->

${categories}

</aiml>`;
  
  fs.writeFileSync(filename, content, 'utf-8');
  console.log(`✓ Generated ${filename}`);
}

function generateSourceAspectAiml(): string {
  const categories: string[] = [];
  
  categories.push(generateAimlCategory(
    'WHAT IS SOURCE',
    sourcePattern.description
  ));
  
  categories.push(generateAimlCategory(
    'EXPLAIN SOURCE',
    `Source is the NonDual Origin - ${sourcePattern.description}. It manifests through 7 aspects: void (ground-source), energy (ground-transformations), pattern (dynamic-order), power (work-rate), transformity (order-accrual), resource (source-distinctions), and autopoiesis (self-creation).`
  ));
  
  // Add queries for each aspect
  Object.entries(sourcePattern.aspects).forEach(([key, aspect]) => {
    categories.push(generateAimlCategory(
      `WHAT IS ${aspect.name}`,
      `${aspect.name} is ${aspect.description}`
    ));
    
    if (aspect.role) {
      categories.push(generateAimlCategory(
        `WHAT IS THE ROLE OF ${aspect.name}`,
        aspect.role
      ));
    }
    
    if (aspect.principle) {
      categories.push(generateAimlCategory(
        `WHAT IS THE PRINCIPLE OF ${aspect.name}`,
        aspect.principle
      ));
    }
  });
  
  return categories.join('\n\n');
}

function generateMasterAiml(aimlFiles: string[]): void {
  const learnTags = aimlFiles.map(file => `  <learn>${file}</learn>`).join('\n');
  
  const content = `<?xml version="1.0" encoding="UTF-8"?>
<aiml version="2.0">
  <!-- Pattern Dynamics 49 Patterns - Master File -->
  <!-- This file loads all 49 pattern AIML files -->

  <!-- Meta information about the system -->
  <category>
    <pattern>WHAT IS PATTERN DYNAMICS</pattern>
    <template>Pattern Dynamics is a comprehensive framework for understanding complex adaptive systems through 49 archetypal patterns. It consists of 1 zeroth-order pattern (Source), 6 first-order patterns (Rhythm, Polarity, Structure, Exchange, Creativity, Dynamics), and 42 second-order patterns formed through holarchical crossing.</template>
  </category>

  <category>
    <pattern>HOW MANY PATTERNS ARE THERE</pattern>
    <template>There are 49 patterns in total: 1 zeroth-order pattern (Source), 6 first-order patterns, and 42 second-order patterns organized into 7 families.</template>
  </category>

  <category>
    <pattern>LIST ALL PATTERNS</pattern>
    <template>The 49 Pattern Dynamics patterns include: Source (zeroth-order); Rhythm, Polarity, Structure, Exchange, Creativity, Dynamics (first-order); and 42 second-order patterns organized into 7 families (Source, Dynamics, Creativity, Exchange, Structure, Polarity, Rhythm families). Each family contains 6 patterns.</template>
  </category>

  <category>
    <pattern>WHAT ARE THE FIRST ORDER PATTERNS</pattern>
    <template>The 6 first-order patterns are: Rhythm (temporality), Polarity (differentiation), Structure (spatiality), Exchange (relationality), Creativity (generativity), and Dynamics (processuality).</template>
  </category>

  <category>
    <pattern>WHAT IS INTEGRAL SEMIOTIC REALISM</pattern>
    <template>Integral Semiotic Realism (ISR) is the theoretical framework integrating Peircean Semiotics (Sign-Object-Interpretant), Critical Realism (stratified ontology), Integral Theory (multi-perspectival AQAL), and Nondual Philosophy (NonDual Origin as source). Each pattern is structured through this ISR lens.</template>
  </category>

  <!-- Load all pattern files -->
${learnTags}

</aiml>`;
  
  const filename = '/home/runner/work/p49/p49/aiml/patterns-49-master.aiml';
  fs.writeFileSync(filename, content, 'utf-8');
  console.log(`✓ Generated ${filename}`);
}

// Main generation function
function main() {
  console.log('Generating AIML files for 49 Pattern Dynamics patterns...\n');
  
  const aimlDir = '/home/runner/work/p49/p49/aiml';
  
  // Ensure directory exists
  if (!fs.existsSync(aimlDir)) {
    fs.mkdirSync(aimlDir, { recursive: true });
  }
  
  const generatedFiles: string[] = [];
  
  // Generate source pattern file
  const sourceContent = `<?xml version="1.0" encoding="UTF-8"?>
<aiml version="2.0">
  <!-- Source Pattern (Zeroth-Order) -->
  <!-- The foundational ground from which all patterns emerge -->

${generateSourceAspectAiml()}

</aiml>`;
  
  const sourceFile = path.join(aimlDir, '00-source-pattern.aiml');
  fs.writeFileSync(sourceFile, sourceContent, 'utf-8');
  console.log(`✓ Generated ${sourceFile}`);
  generatedFiles.push('00-source-pattern.aiml');
  
  // Generate first-order patterns file
  const firstOrderFile = path.join(aimlDir, '01-first-order-patterns.aiml');
  generateAimlFile(firstOrderPatterns, firstOrderFile, 'First-Order Patterns (6 patterns)');
  generatedFiles.push('01-first-order-patterns.aiml');
  
  // Generate second-order pattern files by family
  const families = {
    'source': secondOrderPatterns.slice(0, 6),
    'dynamics': secondOrderPatterns.slice(6, 12),
    'creativity': secondOrderPatterns.slice(12, 18),
    'exchange': secondOrderPatterns.slice(18, 24),
    'structure': secondOrderPatterns.slice(24, 30),
    'polarity': secondOrderPatterns.slice(30, 36),
    'rhythm': secondOrderPatterns.slice(36, 42),
  };
  
  let fileIndex = 2;
  Object.entries(families).forEach(([familyName, patterns]) => {
    const filename = path.join(aimlDir, `${String(fileIndex).padStart(2, '0')}-${familyName}-family.aiml`);
    generateAimlFile(patterns, filename, `${familyName.charAt(0).toUpperCase() + familyName.slice(1)} Family (6 second-order patterns)`);
    generatedFiles.push(`${String(fileIndex).padStart(2, '0')}-${familyName}-family.aiml`);
    fileIndex++;
  });
  
  // Generate master file
  generateMasterAiml(generatedFiles);
  
  console.log('\n✅ Successfully generated all AIML files!');
  console.log(`\nTotal files: ${generatedFiles.length + 1}`);
  console.log('- 1 source pattern file');
  console.log('- 1 first-order patterns file');
  console.log('- 7 second-order family files');
  console.log('- 1 master file\n');
}

// Run the generator
main();
