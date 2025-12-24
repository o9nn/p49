import { motion } from 'framer-motion'
import { useState } from 'react'

interface Node {
  id: string
  x: number
  y: number
  label: string
  color: string
  perspective: string
  domain: string
  description: string
}

interface InfinityLoopProps {
  onNodeClick: (node: Node) => void
  selectedNode: string | null
}

export function InfinityLoop({ onNodeClick, selectedNode }: InfinityLoopProps) {
  const [hoveredNode, setHoveredNode] = useState<string | null>(null)
  const [hoveredPath, setHoveredPath] = useState<string | null>(null)

  const width = 800
  const height = 600
  const centerX = width / 2
  const centerY = height / 2

  const nodes: Node[] = [
    {
      id: 'firstness',
      x: centerX - 200,
      y: centerY - 50,
      label: 'Firstness/Sign',
      color: 'var(--firstness)',
      perspective: '1st Person Perspective/Subject',
      domain: '[epistemology]',
      description: 'The immediate, qualitative aspect of experience. Pure possibility and potentiality before interpretation.'
    },
    {
      id: 'secondness',
      x: centerX + 200,
      y: centerY - 50,
      label: 'Secondness/Object',
      color: 'var(--secondness)',
      perspective: '3rd Person Perspective/Object',
      domain: '[ontology]',
      description: 'The actual, brute fact of existence. Reality as resistance, reaction, and concrete particularity.'
    },
    {
      id: 'thirdness',
      x: centerX,
      y: centerY - 180,
      label: 'Thirdness/Interpretant',
      color: 'var(--thirdness)',
      perspective: '2nd Person Perspective',
      domain: '[methodology]',
      description: 'The mediating principle of meaning, habit, and general law. The interpretation that connects sign and object.'
    }
  ]

  const infinityPath = `
    M ${centerX - 220} ${centerY}
    C ${centerX - 220} ${centerY - 120}, ${centerX - 80} ${centerY - 200}, ${centerX} ${centerY - 200}
    C ${centerX + 80} ${centerY - 200}, ${centerX + 220} ${centerY - 120}, ${centerX + 220} ${centerY}
    C ${centerX + 220} ${centerY + 120}, ${centerX + 80} ${centerY + 200}, ${centerX} ${centerY + 200}
    C ${centerX - 80} ${centerY + 200}, ${centerX - 220} ${centerY + 120}, ${centerX - 220} ${centerY}
  `

  const pathLabels = [
    { id: 'epistemic', text: 'epistemic emergence', path: 0.15, side: 'top' },
    { id: 'continuous-top', text: 'continuous signification (method)', path: 0.4, side: 'top' },
    { id: 'ontological', text: 'ontological emergence', path: 0.65, side: 'top' },
    { id: 'nondual-right', text: 'nondual return', path: 0.85, side: 'right' },
    { id: 'continuous-bottom', text: 'continuous signification (method)', path: 0.5, side: 'bottom' },
    { id: 'nondual-left', text: 'nondual return', path: 0.15, side: 'left' }
  ]

  return (
    <svg
      viewBox={`0 0 ${width} ${height}`}
      className="w-full h-full"
      style={{ maxHeight: '600px' }}
    >
      <defs>
        <linearGradient id="loopGradient" x1="0%" y1="0%" x2="100%" y2="100%">
          <stop offset="0%" stopColor="var(--firstness)" stopOpacity="0.3" />
          <stop offset="50%" stopColor="var(--thirdness)" stopOpacity="0.3" />
          <stop offset="100%" stopColor="var(--secondness)" stopOpacity="0.3" />
        </linearGradient>
        <filter id="glow">
          <feGaussianBlur stdDeviation="3" result="coloredBlur" />
          <feMerge>
            <feMergeNode in="coloredBlur" />
            <feMergeNode in="SourceGraphic" />
          </feMerge>
        </filter>
      </defs>

      <motion.path
        d={infinityPath}
        fill="none"
        stroke="url(#loopGradient)"
        strokeWidth="3"
        initial={{ pathLength: 0, opacity: 0 }}
        animate={{ pathLength: 1, opacity: 1 }}
        transition={{ duration: 2, ease: 'easeInOut' }}
      />

      <motion.path
        d={infinityPath}
        fill="none"
        stroke="oklch(0.35 0.12 270 / 0.15)"
        strokeWidth="2"
        strokeDasharray="5,5"
        initial={{ pathLength: 0 }}
        animate={{ pathLength: 1 }}
        transition={{ duration: 2, ease: 'easeInOut', delay: 0.3 }}
      />

      <g>
        <motion.text
          x={centerX}
          y={centerY + 30}
          textAnchor="middle"
          className="text-lg font-semibold fill-primary"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 1 }}
        >
          Semiotic
        </motion.text>
        <motion.text
          x={centerX}
          y={centerY + 50}
          textAnchor="middle"
          className="text-xs fill-muted-foreground"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 1.2 }}
        >
          [Intransitive Domain]
        </motion.text>
        <motion.text
          x={centerX}
          y={centerY + 65}
          textAnchor="middle"
          className="text-xs fill-muted-foreground italic"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 1.2 }}
        >
          (Zone of Subsistence)
        </motion.text>
      </g>

      <g>
        <motion.text
          x={centerX}
          y={centerY - 220}
          textAnchor="middle"
          className="text-xs fill-muted-foreground"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 1.4 }}
        >
          Cosmic Habits • Dynamic Patterns
        </motion.text>
        <motion.text
          x={centerX}
          y={centerY - 205}
          textAnchor="middle"
          className="text-xs fill-muted-foreground"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 1.4 }}
        >
          Meta-types • Complex Perspectival Systems
        </motion.text>
      </g>

      <g>
        <motion.text
          x={centerX}
          y={centerY + 250}
          textAnchor="middle"
          className="text-lg font-semibold fill-primary"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 1.6 }}
        >
          Enactment
        </motion.text>
        <motion.text
          x={centerX}
          y={centerY + 268}
          textAnchor="middle"
          className="text-xs fill-muted-foreground"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 1.6 }}
        >
          [Empirical Domain]
        </motion.text>
        <motion.text
          x={centerX}
          y={centerY + 283}
          textAnchor="middle"
          className="text-xs fill-muted-foreground italic"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 1.6 }}
        >
          (Zone of Existence)
        </motion.text>
      </g>

      <motion.text
        x={centerX - 280}
        y={centerY}
        textAnchor="middle"
        className="text-sm font-medium fill-foreground italic"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 1.8 }}
      >
        NonDual Evolution
      </motion.text>

      <motion.text
        x={centerX + 280}
        y={centerY}
        textAnchor="middle"
        className="text-sm font-medium fill-foreground italic"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 1.8 }}
      >
        NonDual Evolution
      </motion.text>

      <g>
        <text x={centerX - 280} y={centerY + 18} textAnchor="middle" className="text-[10px] fill-muted-foreground">
          NonDual Origin
        </text>
        <text x={centerX + 280} y={centerY + 18} textAnchor="middle" className="text-[10px] fill-muted-foreground">
          NonDual Origin
        </text>
      </g>

      <motion.text
        x={centerX - 120}
        y={centerY + 150}
        textAnchor="middle"
        className="text-[11px] fill-muted-foreground"
        initial={{ opacity: 0 }}
        animate={{ opacity: 0.7 }}
        transition={{ delay: 2 }}
      >
        nondual return
      </motion.text>

      <motion.text
        x={centerX + 120}
        y={centerY + 150}
        textAnchor="middle"
        className="text-[11px] fill-muted-foreground"
        initial={{ opacity: 0 }}
        animate={{ opacity: 0.7 }}
        transition={{ delay: 2 }}
      >
        nondual return
      </motion.text>

      <motion.text
        x={centerX - 100}
        y={centerY - 145}
        textAnchor="middle"
        className="text-[11px] fill-muted-foreground"
        initial={{ opacity: 0 }}
        animate={{ opacity: 0.7 }}
        transition={{ delay: 2 }}
      >
        epistemic emergence
      </motion.text>

      <motion.text
        x={centerX + 100}
        y={centerY - 145}
        textAnchor="middle"
        className="text-[11px] fill-muted-foreground"
        initial={{ opacity: 0 }}
        animate={{ opacity: 0.7 }}
        transition={{ delay: 2 }}
      >
        ontological emergence
      </motion.text>

      <motion.text
        x={centerX}
        y={centerY - 165}
        textAnchor="middle"
        className="text-[11px] fill-muted-foreground"
        initial={{ opacity: 0 }}
        animate={{ opacity: 0.7 }}
        transition={{ delay: 2 }}
      >
        continuous signification (method)
      </motion.text>

      <motion.text
        x={centerX}
        y={centerY + 190}
        textAnchor="middle"
        className="text-[11px] fill-muted-foreground"
        initial={{ opacity: 0 }}
        animate={{ opacity: 0.7 }}
        transition={{ delay: 2 }}
      >
        continuous signification (method)
      </motion.text>

      <g>
        {nodes.map((node, index) => {
          const isHovered = hoveredNode === node.id
          const isSelected = selectedNode === node.id
          const scale = isHovered || isSelected ? 1.15 : 1

          return (
            <motion.g
              key={node.id}
              initial={{ opacity: 0, scale: 0 }}
              animate={{ opacity: 1, scale: 1 }}
              transition={{ delay: 0.5 + index * 0.2, type: 'spring', stiffness: 200 }}
              style={{ cursor: 'pointer' }}
              onMouseEnter={() => setHoveredNode(node.id)}
              onMouseLeave={() => setHoveredNode(null)}
              onClick={() => onNodeClick(node)}
            >
              <motion.circle
                cx={node.x}
                cy={node.y}
                r={45}
                fill={isSelected ? node.color : 'var(--card)'}
                stroke={node.color}
                strokeWidth={isSelected ? 4 : 3}
                animate={{ scale }}
                transition={{ type: 'spring', stiffness: 300 }}
                filter={isHovered || isSelected ? 'url(#glow)' : undefined}
              />
              <motion.text
                x={node.x}
                y={node.y - 5}
                textAnchor="middle"
                className="text-sm font-semibold pointer-events-none"
                fill={isSelected ? 'var(--card)' : node.color}
                animate={{ scale }}
                transition={{ type: 'spring', stiffness: 300 }}
              >
                {node.label.split('/')[0]}
              </motion.text>
              <motion.text
                x={node.x}
                y={node.y + 10}
                textAnchor="middle"
                className="text-xs pointer-events-none"
                fill={isSelected ? 'var(--card)' : node.color}
                animate={{ scale }}
                transition={{ type: 'spring', stiffness: 300 }}
              >
                {node.label.split('/')[1]}
              </motion.text>
              <text
                x={node.x}
                y={node.y + 65}
                textAnchor="middle"
                className="text-[10px] fill-muted-foreground pointer-events-none"
              >
                {node.perspective}
              </text>
              <text
                x={node.x}
                y={node.y + 78}
                textAnchor="middle"
                className="text-[9px] fill-muted-foreground pointer-events-none"
              >
                {node.domain}
              </text>
            </motion.g>
          )
        })}
      </g>

      <text
        x={centerX - 120}
        y={centerY + 110}
        textAnchor="middle"
        className="text-xs fill-muted-foreground"
      >
        [Actual Domain]
      </text>
      <text
        x={centerX + 120}
        y={centerY + 110}
        textAnchor="middle"
        className="text-xs fill-muted-foreground"
      >
        [Actual Domain]
      </text>
    </svg>
  )
}
