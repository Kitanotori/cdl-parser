/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

import cdl.javacompat.CDLConversions

/**
 * @param rlabel realization label of an entity
 * @param dlabel definition label of an entity. For UWs this is the headword.
 * @param entities list of subentities
 * @param arcs list of arcs connecting to other entities
 * @param modifiers
 */
class Statement(override val rlabel: String = "", deflabel: DefinitionLabel = DefinitionLabel.Null,
                val entities: List[Entity] = Nil, val arcs: List[Arc] = Nil) extends DefinitionLabel(deflabel.dlabel, deflabel.dtype) with Entity {

  def this(concepts: List[Concept], arcs: List[Arc]) = this("#", DefinitionLabel.Null, concepts, arcs)

  def this(concepts: java.util.List[Concept], arcs: java.util.List[Arc]) = this("#", DefinitionLabel.Null, CDLConversions.toList(concepts), CDLConversions.toList(arcs))

  def getRLabelName = {
    if (rlabel.isEmpty) java.util.UUID.randomUUID().toString
    else rlabel
  }

  override def toString: String = getNormalString("")

  def getNormalString(tabs: String) = {
    val s = new StringBuilder(500)
    for (e <- entities) {
      s ++= e.toString(tabs + '\t') + '\n'
    }
    for (a <- arcs) {
      s ++= a.toString(tabs + '\t') + '\n'
    }
    if (s.isEmpty) {
      tabs + '{' + rlabel + dlabel + '}'
    } else {
      tabs + '{' + rlabel + dlabel + '\n' + s.toString + tabs + '}'
    }
  }

}