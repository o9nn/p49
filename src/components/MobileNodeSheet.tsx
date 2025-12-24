import { motion } from 'framer-motion'
import {
  Sheet,
  SheetContent,
  SheetHeader,
  SheetTitle,
} from './ui/sheet'
import { Badge } from './ui/badge'
import { Separator } from './ui/separator'

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

interface MobileNodeSheetProps {
  node: Node | null
  open: boolean
  onClose: () => void
}

export function MobileNodeSheet({ node, open, onClose }: MobileNodeSheetProps) {
  const getExtendedInfo = (nodeId: string) => {
    switch (nodeId) {
      case 'firstness':
        return {
          title: 'Firstness / Sign',
          subtitle: '1st Person Perspective - Subject',
          domain: 'Epistemology',
          aspectType: 'Actual Domain',
          details: [
            'Represents pure qualitative possibility and immediate experience',
            'The realm of feelings, sensations, and pure potentiality',
            'Exists as possibility before any actualization or interpretation',
            'Associated with the subjective, phenomenal aspect of experience'
          ],
          characteristics: [
            'Epistemic emergence from NonDual origin',
            'Sign-based understanding',
            'First-person subjective experience',
            'Quality without relation'
          ]
        }
      case 'secondness':
        return {
          title: 'Secondness / Object',
          subtitle: '3rd Person Perspective - Object',
          domain: 'Ontology',
          aspectType: 'Actual Domain',
          details: [
            'Represents brute actuality and concrete existence',
            'The realm of facts, forces, and resistant reality',
            'Characterized by opposition, reaction, and otherness',
            'Associated with objective reality independent of mind'
          ],
          characteristics: [
            'Ontological emergence from NonDual origin',
            'Object-based reality',
            'Third-person objective perspective',
            'Actuality through resistance'
          ]
        }
      case 'thirdness':
        return {
          title: 'Thirdness / Interpretant',
          subtitle: '2nd Person Perspective - Methodology',
          domain: 'Methodology',
          aspectType: 'Meta-level Framework',
          details: [
            'Represents mediation, generality, and lawful continuity',
            'The realm of meaning, habit, and interpretation',
            'Connects sign and object through interpretive process',
            'Associated with intersubjective understanding and communication'
          ],
          characteristics: [
            'Continuous signification as method',
            'Interpretant-based meaning making',
            'Second-person dialogical perspective',
            'Generality through habit and law'
          ]
        }
      default:
        return null
    }
  }

  if (!node) return null

  return (
    <Sheet open={open} onOpenChange={onClose}>
      <SheetContent side="bottom" className="h-[85vh]">
        <SheetHeader className="mb-4">
          <div className="flex items-center gap-2 mb-2">
            <div
              className="w-3 h-3 rounded-full"
              style={{ backgroundColor: node.color }}
            />
            <Badge variant="secondary" className="text-xs">
              {getExtendedInfo(node.id)?.domain}
            </Badge>
          </div>
          <SheetTitle className="text-xl" style={{ color: node.color }}>
            {getExtendedInfo(node.id)?.title}
          </SheetTitle>
          <p className="text-sm text-muted-foreground">
            {getExtendedInfo(node.id)?.subtitle}
          </p>
        </SheetHeader>

        <div className="space-y-4 overflow-auto h-[calc(100%-120px)] pb-6">
          <div>
            <h3 className="text-sm font-semibold mb-2 flex items-center gap-2">
              <span
                className="w-1 h-4 rounded-full"
                style={{ backgroundColor: node.color }}
              />
              Core Description
            </h3>
            <p className="text-sm leading-relaxed text-foreground/90">
              {node.description}
            </p>
          </div>

          <Separator className="my-4" />

          <div>
            <h3 className="text-sm font-semibold mb-3 flex items-center gap-2">
              <span
                className="w-1 h-4 rounded-full"
                style={{ backgroundColor: node.color }}
              />
              Key Aspects
            </h3>
            <ul className="space-y-2">
              {getExtendedInfo(node.id)?.details.map((detail, index) => (
                <motion.li
                  key={index}
                  initial={{ opacity: 0, x: -10 }}
                  animate={{ opacity: 1, x: 0 }}
                  transition={{ delay: index * 0.1 }}
                  className="text-sm text-foreground/80 flex gap-2"
                >
                  <span className="text-muted-foreground mt-1.5">•</span>
                  <span className="flex-1 leading-relaxed">{detail}</span>
                </motion.li>
              ))}
            </ul>
          </div>

          <Separator className="my-4" />

          <div>
            <h3 className="text-sm font-semibold mb-3 flex items-center gap-2">
              <span
                className="w-1 h-4 rounded-full"
                style={{ backgroundColor: node.color }}
              />
              Characteristics
            </h3>
            <div className="space-y-2">
              {getExtendedInfo(node.id)?.characteristics.map((char, index) => (
                <motion.div
                  key={index}
                  initial={{ opacity: 0, scale: 0.9 }}
                  animate={{ opacity: 1, scale: 1 }}
                  transition={{ delay: 0.3 + index * 0.05 }}
                >
                  <Badge
                    variant="outline"
                    className="w-full justify-start text-xs py-2 px-3"
                    style={{
                      borderColor: node.color,
                      color: node.color
                    }}
                  >
                    {char}
                  </Badge>
                </motion.div>
              ))}
            </div>
          </div>

          <Separator className="my-4" />

          <div className="bg-muted/30 rounded-lg p-4">
            <p className="text-xs text-muted-foreground leading-relaxed">
              <span className="font-semibold">Domain Context:</span>{' '}
              {getExtendedInfo(node.id)?.aspectType}
            </p>
          </div>
        </div>
      </SheetContent>
    </Sheet>
  )
}
