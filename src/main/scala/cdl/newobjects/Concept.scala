package cdl.newobjects

/*
 * There are 5 possible types of concepts:
 * 
 * (1) Concept in which content itself includes meaning (Textual Concept)
 * (2) Content surface notation as concept (Literal Concept)
 * (3) Temporary concept (or anonymous concept)
 * (4) Concept with only one realization (Proper Concept)
 * (5) Concept in which the realization notation itself is the primary concept definition (Identifier Concept)
 */
object ConceptType extends Enumeration {
  val Textual, Literal, Temporary, Proper, Identifier = Value
}

/*
 * RealizationLabel ::= #ArbitraryCharacterString
 */
class RealizationLabel(var rl: String = "") {
  def isEmpty = rl.isEmpty
  def nonEmpty = rl.nonEmpty

  override def toString = rl
}

/*
 * DefinitionLabel ::= DefinitionLabelDefinedInCDD
 */
class DefinitionLabel(var dl: String = "") {
  def isEmpty = dl.isEmpty
  def nonEmpty = dl.nonEmpty

  override def toString = dl
}

/*
 * Domain model of CDL
 */

trait Concept {
  def rlabel: RealizationLabel
  def dlabel: DefinitionLabel
  def attrs: List[Attribute]
  def ctype: ConceptType.Value = ConceptType.Textual // Is not yet implemented properly anywhere
  override def toString: String = "{#"+rlabel+" "+dlabel+" }" // Remember to override!
}

trait Entity extends Concept {
}

class ElementalEntity(
  //override val ctype: ConceptType.Value,
  override val rlabel: RealizationLabel,
  override val dlabel: DefinitionLabel,
  override val attrs: List[Attribute] = Nil)
  extends Entity {

  override def toString: String = "<"+rlabel+":"+dlabel + attrs.mkString(".", ".", "")+">"
}

class ComplexEntity(
  //override val ctype: ConceptType.Value,
  override val rlabel: RealizationLabel,
  override val dlabel: DefinitionLabel,
  override val attrs: List[Attribute] = Nil,
  val entities: List[Concept] = Nil, // TODO: This should be List[Entity] !
  val relations: List[Relation] = Nil)
  extends Entity {

  override def toString: String = toString("")

  private def toString(tabs: String): String = {
    val s = new StringBuilder(500)

    entities.foreach(_ match {
      case e: ElementalEntity => s ++= e.toString + '\n'
      case e: ComplexEntity   => s ++= e.toString(tabs + '\t') + '\n'
    })

    relations.foreach(r => s ++= r.toString(tabs + '\t') + '\n')

    if (s.isEmpty) {
      tabs + '{' + rlabel + dlabel + '}'
    } else {
      tabs + '{' + rlabel + dlabel + '\n' + s.toString + tabs + '}'
    }
  }

  //def this(concepts: List[Concept], arcs: List[Arc]) = this("#", DefinitionLabel.Null, concepts, arcs)

  //def this(concepts: java.util.List[Concept], arcs: java.util.List[Arc]) = this("#", DefinitionLabel.Null, CDLConversions.toList(concepts), CDLConversions.toList(arcs))

  def getRLabelName = {
    if (rlabel.isEmpty) java.util.UUID.randomUUID.toString
    else rlabel
  }
}

trait Relation extends Concept {
  def from: RealizationLabel
  def relation: DefinitionLabel
  def to: RealizationLabel

  override def toString = toString("")
  def toString(tabs: String) = tabs+"["+from+" "+relation+" "+to+"]"
  def toCypherString = "x"+from+"-[:"+relation+"]->x"+to
}

class ElementalRelation(
  //override val ctype: ConceptType.Value,
  override val from: RealizationLabel,
  override val relation: DefinitionLabel,
  override val to: RealizationLabel,
  override val rlabel: RealizationLabel = new RealizationLabel(),
  override val dlabel: DefinitionLabel = new DefinitionLabel(),
  override val attrs: List[Attribute] = Nil)
  extends Relation {

  def this(from: String, rel: String, to: String) = this(new RealizationLabel(from), new DefinitionLabel(rel), new RealizationLabel(to))
}

//class ComplexRelation extends Relation {
// TODO: implement later!
//}

class Attribute(
  //override val ctype: ConceptType.Value = ConceptType.Textual,
  override val dlabel: DefinitionLabel,
  override val rlabel: RealizationLabel = new RealizationLabel(),
  override val attrs: List[Attribute] = Nil)
  extends Concept {

  def this(attr: String) {
    this(new DefinitionLabel(attr))
  }

  override def toString = dlabel.toString
  def toAttrListForm = ".@"+toString
}

class UW(
  //ctype: ConceptType.Value = ConceptType.Proper,
  rlabel: RealizationLabel,
  val hw: String,
  val cons: List[Constraint],
  override val attrs: List[Attribute])
  extends ElementalEntity(rlabel, new DefinitionLabel(hw + cons.mkString("(", ",", ")"))) {

  def this(hw: String, cons: List[Constraint], attrs: List[Attribute] = Nil) = this(new RealizationLabel(), hw, cons, attrs)

  def uw = dlabel.toString
}

/*
 * By specification Constraint is actually ComplexEntity, so this is slight simplification
 */
class Constraint(val rel: DefinitionLabel, val direction: String, val uw: UW) {
  //def this(c: String) {  }
  override def toString: String = rel + direction + uw.toString
}

class CDLDocument(
  ent: List[Concept] = Nil,
  rl: RealizationLabel = new RealizationLabel(),
  dl: DefinitionLabel = new DefinitionLabel(),
  atr: List[Attribute] = Nil,
  rel: List[Relation] = Nil) extends ComplexEntity(rl, dl, atr, ent, rel) {

  // def this(entities: java.util.List[Entity], title: String) = this(CDLConversions.toList(entities), title)

  override def toString = {
    val res = new StringBuilder(400000)
    entities.map(e => e.toString).addString(res, "\n")
    res.toString
  }
}