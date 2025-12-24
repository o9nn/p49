import { useState } from 'react'
import { motion } from 'framer-motion'
import { InfinityLoop } from './components/InfinityLoop'
import { NodeDetailPanel } from './components/NodeDetailPanel'
import { MobileNodeSheet } from './components/MobileNodeSheet'
import { PatternRelationalExplorer } from './components/PatternRelationalExplorer'
import { RootedTrees48Explorer } from './components/RootedTrees48Explorer'
import { Card } from './components/ui/card'
import { Button } from './components/ui/button'
import { Tabs, TabsList, TabsTrigger, TabsContent } from './components/ui/tabs'
import { ArrowsClockwise, Info } from '@phosphor-icons/react'
import { useIsMobile } from './hooks/use-mobile'
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from './components/ui/tooltip'
import diagramImage from '@/assets/images/Pattern-Rosetta-Stone.png'

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

function App() {
  const [selectedNode, setSelectedNode] = useState<Node | null>(null)
  const [mobileSheetOpen, setMobileSheetOpen] = useState(false)
  const isMobile = useIsMobile()

  const handleNodeClick = (node: Node) => {
    setSelectedNode(node)
    if (isMobile) {
      setMobileSheetOpen(true)
    }
  }

  const handleClose = () => {
    setSelectedNode(null)
    setMobileSheetOpen(false)
  }

  const handleReset = () => {
    setSelectedNode(null)
    setMobileSheetOpen(false)
  }

  return (
    <TooltipProvider>
      <div className="min-h-screen bg-background p-4 md:p-6">
        <div className="max-w-7xl mx-auto">
          <motion.div
            initial={{ opacity: 0, y: -20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6 }}
            className="text-center mb-8"
          >
            <h1 className="text-3xl md:text-4xl font-semibold mb-3 text-primary" style={{ fontFamily: 'var(--font-playfair)' }}>
              Integral Semiotic Realism
            </h1>
            <p className="text-sm md:text-base text-muted-foreground max-w-2xl mx-auto leading-relaxed">
              An interactive exploration of Pattern Dynamics through the triadic relationship of Sign, Object, and Interpretant
            </p>
          </motion.div>

          <Tabs defaultValue="infinity-loop" className="w-full">
            <TabsList className="grid w-full max-w-3xl mx-auto grid-cols-3 mb-8">
              <TabsTrigger value="infinity-loop">Infinity Loop</TabsTrigger>
              <TabsTrigger value="relational">Relational Matrix</TabsTrigger>
              <TabsTrigger value="rooted-trees">Rooted Trees 48</TabsTrigger>
            </TabsList>

            <TabsContent value="infinity-loop" className="space-y-8">
              <div className="relative">
                <motion.div
                  initial={{ opacity: 0, scale: 0.95 }}
                  animate={{ opacity: 1, scale: 1 }}
                  transition={{ duration: 0.8, delay: 0.2 }}
                >
                  <Card className="p-4 md:p-8 shadow-xl">
                    <InfinityLoop
                      onNodeClick={handleNodeClick}
                      selectedNode={selectedNode?.id || null}
                    />
                  </Card>
                </motion.div>

                {!isMobile && (
                  <NodeDetailPanel
                    node={selectedNode}
                    onClose={handleClose}
                  />
                )}
              </div>

              <motion.div
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                transition={{ delay: 1 }}
                className="flex flex-col md:flex-row gap-4 items-center justify-center"
              >
                <Button
                  variant="outline"
                  onClick={handleReset}
                  className="gap-2"
                >
                  <ArrowsClockwise size={18} />
                  Reset View
                </Button>

                <Tooltip>
                  <TooltipTrigger asChild>
                    <Button variant="ghost" size="icon">
                      <Info size={20} />
                    </Button>
                  </TooltipTrigger>
                  <TooltipContent className="max-w-xs">
                    <p className="text-xs">
                      Click on any node (Firstness, Secondness, or Thirdness) to explore its philosophical significance and role in the semiotic process.
                    </p>
                  </TooltipContent>
                </Tooltip>
              </motion.div>

              <motion.div
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                transition={{ delay: 1.5 }}
                className="grid md:grid-cols-3 gap-6"
              >
                <Card className="p-6 border-2 hover:shadow-lg transition-shadow" style={{ borderColor: 'var(--firstness)' }}>
                  <div className="flex items-center gap-2 mb-3">
                    <div className="w-3 h-3 rounded-full" style={{ backgroundColor: 'var(--firstness)' }} />
                    <h3 className="font-semibold" style={{ color: 'var(--firstness)' }}>Actual Domain</h3>
                  </div>
                  <p className="text-sm text-muted-foreground leading-relaxed">
                    The domain of manifest experience and concrete phenomena, where both epistemic signs and ontological objects exist in their actualized forms.
                  </p>
                </Card>

                <Card className="p-6 border-2 hover:shadow-lg transition-shadow" style={{ borderColor: 'var(--thirdness)' }}>
                  <div className="flex items-center gap-2 mb-3">
                    <div className="w-3 h-3 rounded-full" style={{ backgroundColor: 'var(--thirdness)' }} />
                    <h3 className="font-semibold" style={{ color: 'var(--thirdness)' }}>Intransitive Domain</h3>
                  </div>
                  <p className="text-sm text-muted-foreground leading-relaxed">
                    The zone of subsistence where semiotic processes operate—the realm of pure relationality and meaning-making before full actualization.
                  </p>
                </Card>

                <Card className="p-6 border-2 hover:shadow-lg transition-shadow" style={{ borderColor: 'var(--secondness)' }}>
                  <div className="flex items-center gap-2 mb-3">
                    <div className="w-3 h-3 rounded-full" style={{ backgroundColor: 'var(--secondness)' }} />
                    <h3 className="font-semibold" style={{ color: 'var(--secondness)' }}>Empirical Domain</h3>
                  </div>
                  <p className="text-sm text-muted-foreground leading-relaxed">
                    The zone of existence where enactment occurs—the realm of lived experience, practice, and the empirical manifestation of meaning.
                  </p>
                </Card>
              </motion.div>

              <motion.div
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                transition={{ delay: 2 }}
                className="text-center"
              >
                <p className="text-xs text-muted-foreground italic">
                  The NonDual Evolution represents the dynamic return to and emergence from the NonDual Origin, the zone of subsistence from which all perspectival distinctions arise.
                </p>
              </motion.div>

              <motion.div
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                transition={{ delay: 2.2 }}
              >
                <Card className="p-6">
                  <h2 className="text-xl font-semibold mb-4 text-center" style={{ fontFamily: 'var(--font-playfair)' }}>
                    Reference Diagram
                  </h2>
                  <div className="flex justify-center">
                    <img 
                      src={diagramImage} 
                      alt="Integral Semiotic Realism - Complete diagram showing the triadic relationship between Firstness/Sign (1st Person Perspective/Subject), Secondness/Object (3rd Person Perspective/Object), and Thirdness/Interpretant (2nd Person Perspective), connected through continuous signification, epistemic emergence, ontological emergence, and nondual return paths across Actual, Intransitive, and Empirical domains"
                      className="max-w-full h-auto rounded-lg border border-border"
                    />
                  </div>
                  <p className="text-xs text-muted-foreground text-center mt-4 italic">
                    Static reference showing the complete Integral Semiotic Realism framework with all nodes, paths, domains, and perspectives
                  </p>
                </Card>
              </motion.div>
            </TabsContent>

            <TabsContent value="relational">
              <PatternRelationalExplorer />
            </TabsContent>

            <TabsContent value="rooted-trees">
              <RootedTrees48Explorer />
            </TabsContent>
          </Tabs>
        </div>

        {isMobile && (
          <MobileNodeSheet
            node={selectedNode}
            open={mobileSheetOpen}
            onClose={handleClose}
          />
        )}
      </div>
    </TooltipProvider>
  )
}

export default App