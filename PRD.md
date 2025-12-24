# Planning Guide

An interactive visualization exploring the cyclical dynamics of Integral Semiotic Realism through an engaging, explorable infinity loop diagram that reveals the relationships between Firstness, Secondness, Thirdness, and their NonDual origins.

**Experience Qualities**: 
1. **Contemplative** - The interface should invite deep exploration and understanding of complex philosophical concepts through calm, purposeful interactions
2. **Fluid** - Animations and transitions should feel continuous and organic, mirroring the cyclical nature of the semiotic process
3. **Revealing** - Information should progressively disclose itself, rewarding curiosity and engagement with layers of meaning

**Complexity Level**: Light Application (multiple features with basic state)
This is an educational visualization tool with interactive exploration features. It combines dynamic SVG rendering, hover states, click interactions, and information panels without requiring complex state management or user accounts.

## Essential Features

### Interactive Infinity Loop Visualization
- **Functionality**: Renders the infinity loop with three main nodes (Firstness, Secondness, Thirdness), connecting arrows, and labeled paths showing the continuous semiotic cycle
- **Purpose**: Provides a visual anchor for understanding the complex relationships in integral semiotic realism
- **Trigger**: Loads on page mount
- **Progression**: App loads → SVG infinity loop renders with smooth entrance animation → Nodes pulse subtly → User can see the complete diagram
- **Success criteria**: All elements visible, properly positioned, with smooth animations that don't distract

### Node Interaction System
- **Functionality**: Each of the three main nodes (Firstness, Secondness, Thirdness) responds to hover and click, displaying detailed information
- **Purpose**: Allows users to explore the philosophical concepts deeply without overwhelming the initial view
- **Trigger**: User hovers over or clicks a node
- **Progression**: User hovers node → Node highlights and scales slightly → Labels become more prominent → User clicks → Detail panel slides in from side → Shows full description, perspective info, and domain context
- **Success criteria**: Smooth transitions, clear visual feedback, information is readable and well-organized

### Path Labels and Arrows
- **Functionality**: The curved paths between nodes display labels like "continuous signification," "epistemic emergence," "ontological emergence," "nondual return"
- **Purpose**: Reveals the methodological flow and relationships between the three aspects
- **Trigger**: Always visible with enhanced visibility on hover
- **Progression**: Paths render with diagram → User hovers path → Path highlights → Associated text becomes more prominent → Shows the cyclical nature
- **Success criteria**: Text is readable along curves, arrows are clear, hover states are distinct

### Domain and Zone Information
- **Functionality**: Displays contextual information about Actual Domain, Intransitive Domain, Empirical Domain, and Zones (Subsistence, Existence)
- **Purpose**: Provides the ontological and epistemological context for the semiotic framework
- **Trigger**: Information icons or clickable regions on the diagram
- **Progression**: User notices information indicators → Clicks or hovers → Tooltip or panel appears → Shows domain/zone explanation → User can dismiss or continue exploring
- **Success criteria**: Information is accessible but not intrusive, clearly explains the philosophical framework

### Perspective System Display
- **Functionality**: Shows the relationship between 1st person (Subject), 2nd person (Interpretant), and 3rd person (Object) perspectives
- **Purpose**: Highlights the integral perspectival nature of the framework connecting epistemology and ontology
- **Trigger**: Toggle or automatic display in node details
- **Progression**: User explores node → Perspective information is visible in detail panel → Color coding or icons distinguish perspectives → User understands the multi-perspectival nature
- **Success criteria**: Clear visual distinction between perspectives, easy to understand mapping

## Edge Case Handling
- **Mobile/Touch Devices**: Convert hover states to tap interactions, ensure detail panels are appropriately sized for smaller screens
- **Overflow Text**: Long philosophical descriptions are truncated with "read more" expansion to prevent layout breaking
- **Rapid Interaction**: Debounce hover effects to prevent flickering when user moves mouse quickly across nodes
- **No JavaScript**: Fallback to static SVG diagram with all labels visible (graceful degradation)

## Design Direction
The design should feel contemplative, scholarly, and modern—like a digital philosophy text that has transcended the page. It should evoke a sense of intellectual depth while remaining accessible. The interface should be minimal and elegant, allowing the content and diagram to be the hero, with interactions that feel natural and purposeful rather than gimmicky.

## Color Selection
Triadic color scheme representing the three fundamental aspects (Firstness, Secondness, Thirdness) with a neutral foundation for clarity and focus.

- **Primary Color (Deep Indigo)**: `oklch(0.35 0.12 270)` - Represents depth of thought, philosophical inquiry, and the contemplative nature of the content
- **Secondary Colors**: 
  - **Firstness (Warm Amber)**: `oklch(0.70 0.15 70)` - Epistemic, subjective, sign-based
  - **Secondness (Cool Teal)**: `oklch(0.60 0.12 200)` - Ontological, objective, object-based  
  - **Thirdness (Vibrant Purple)**: `oklch(0.55 0.15 300)` - Interpretant, intersubjective, mediating
- **Accent Color (Bright Cyan)**: `oklch(0.75 0.15 210)` - For interactive elements, highlights, and calls to attention
- **Foreground/Background Pairings**:
  - Background (Soft Warm White `oklch(0.98 0.01 80)`): Deep Indigo text `oklch(0.35 0.12 270)` - Ratio 9.2:1 ✓
  - Card (Pure White `oklch(1 0 0)`): Deep Indigo text `oklch(0.35 0.12 270)` - Ratio 10.5:1 ✓
  - Primary (Deep Indigo `oklch(0.35 0.12 270)`): White text `oklch(1 0 0)` - Ratio 10.5:1 ✓
  - Accent (Bright Cyan `oklch(0.75 0.15 210)`): Deep Indigo text `oklch(0.35 0.12 270)` - Ratio 5.1:1 ✓
  - Muted (Light Gray `oklch(0.95 0.005 270)`): Dark Gray text `oklch(0.45 0.02 270)` - Ratio 7.8:1 ✓

## Font Selection
Typography should convey intellectual rigor while maintaining approachability—a serif for headings to evoke scholarly tradition, and a clean sans-serif for body text ensuring maximum readability.

- **Typographic Hierarchy**:
  - H1 (Main Title "Integral Semiotic Realism"): Playfair Display/32px/tight letter spacing/-0.02em/semibold
  - H2 (Node Labels - Firstness, Secondness, Thirdness): Inter/20px/medium/normal spacing
  - H3 (Section Headers in Panels): Inter/16px/semibold/0.01em
  - Body (Descriptions and Explanations): Inter/14px/normal/0.015em/line-height 1.6
  - Labels (Path labels, small annotations): Inter/12px/medium/0.02em/uppercase
  - Caption (Meta information): Inter/11px/normal/0.01em/muted color

## Animations
Animations should feel organic and purposeful, reflecting the continuous cyclical flow of the semiotic process. Motion guides attention and reinforces the dynamic nature of meaning-making without distracting from content comprehension.

- **Purposeful Meaning**: Use flowing, circular motion to reinforce the infinity loop concept; fade transitions for information disclosure mimic the emergence of understanding
- **Hierarchy of Movement**: 
  1. Primary: Infinity loop entrance (draws attention to core diagram)
  2. Secondary: Node interactions (rewards exploration)
  3. Tertiary: Path highlights and subtle pulse effects (ambient life)

## Component Selection
- **Components**: 
  - Custom SVG component for infinity loop diagram (no shadcn equivalent for this specialized visualization)
  - `Card` for detail panels and information displays
  - `Button` for interactive controls (reset view, toggle layers)
  - `Tooltip` for quick contextual information on hover
  - `Sheet` or `Dialog` for expanded node details on mobile
  - `Badge` for labeling perspectives (1st, 2nd, 3rd person) and domains
  - `Separator` to divide sections in detail panels
  - `ScrollArea` for long content in detail panels
  
- **Customizations**: 
  - Custom SVG infinity loop path generator with dynamic node positioning
  - Animated path stroking using framer-motion for entrance effects
  - Custom curved text labels following SVG path curves
  - Gradient fills for the infinity loop to suggest flow direction
  
- **States**: 
  - Nodes: default (visible) → hover (highlighted, scaled 1.1) → active/selected (prominent, detail panel open)
  - Paths: default (subtle) → hover (emphasized stroke, brighter color)
  - Detail panels: hidden → entering (slide + fade) → visible → exiting (fade out)
  - Buttons: default → hover (slight lift, color shift) → active (pressed, scale 0.98)
  
- **Icon Selection**: 
  - `Circle` for nodes (custom styled)
  - `Info` for information indicators
  - `X` for closing detail panels
  - `Eye` for toggling visibility layers
  - `ArrowsClockwise` for reset view
  - `CaretRight` for expanding sections
  
- **Spacing**: 
  - Container padding: `p-6` (24px) on desktop, `p-4` (16px) on mobile
  - Card padding: `p-6`
  - Section gaps: `gap-8` between major sections, `gap-4` within sections
  - Element spacing: `space-y-4` for vertical stacks, `gap-3` for inline elements
  
- **Mobile**: 
  - Stack detail panels below diagram on small screens (<768px)
  - Convert hover interactions to tap/touch
  - Simplify infinity loop to fit portrait orientation
  - Use Sheet component instead of side panels for node details
  - Reduce font sizes by 10-15% for small screens
  - Ensure minimum touch target size of 44x44px for all interactive elements
