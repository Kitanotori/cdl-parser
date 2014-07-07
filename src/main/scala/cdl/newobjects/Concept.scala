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
class RealizationLabel(rl: String = "") {
  def isEmpty = rl.isEmpty
  def nonEmpty = rl.nonEmpty

  override def toString = rl
  override def equals(obj: Any): Boolean = obj.toString == toString
}

/*
 * DefinitionLabel ::= DefinitionLabelDefinedInCDD
 */
class DefinitionLabel(dl: String = "") {
  def isEmpty = dl.isEmpty
  def nonEmpty = dl.nonEmpty

  override def toString = dl
  override def equals(obj: Any): Boolean = obj.toString == toString
}

/*
 * Domain model of CDL
 */

trait Concept {
  def rlabel: RealizationLabel
  def dlabel: DefinitionLabel
  def attrs: List[Attribute]
  def ctype: ConceptType.Value = ConceptType.Textual // Is not yet implemented properly anywhere

  override def toString: String = "{#" + rlabel + " " + dlabel + " }" // Remember to !
  override def equals(obj: Any): Boolean = obj.toString == toString
}

trait Entity extends Concept {
}

class ElementalEntity(
  // val ctype: ConceptType.Value,
  val rlabel: RealizationLabel,
  val dlabel: DefinitionLabel,
  val attrs: List[Attribute])
  extends Entity {

  override def toString: String = {
    val a = if (attrs.isEmpty) "" else attrs.mkString(".@", ".@", "")
    "<" + rlabel + ":" + dlabel + a + ">"
  }
}

class ComplexEntity(
  // val ctype: ConceptType.Value,
  val rlabel: RealizationLabel,
  val dlabel: DefinitionLabel,
  val attrs: List[Attribute] = Nil,
  val entities: List[Concept] = Nil, // TODO: This should be List[Entity] !
  val relations: List[Relation] = Nil)
  extends Entity {

  override def toString: String = toString("")

  private def toString(tabs: String): String = {
    val s = new StringBuilder(500)

    entities.foreach(_ match {
      case e: ElementalEntity => s ++= e.toString + '\n'
      case e: ComplexEntity => s ++= e.toString(tabs + '\t') + '\n'
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
  def toString(tabs: String) = tabs + "[" + from + " " + relation + " " + to + "]"
  def toCypherString = "x" + from + "-[:" + relation + "]->x" + to
}

class ElementalRelation(
  // val ctype: ConceptType.Value,
  val from: RealizationLabel,
  val relation: DefinitionLabel,
  val to: RealizationLabel,
  val rlabel: RealizationLabel = new RealizationLabel(),
  val dlabel: DefinitionLabel = new DefinitionLabel(),
  val attrs: List[Attribute] = Nil)
  extends Relation {

  def this(from: String, rel: String, to: String) = this(new RealizationLabel(from), new DefinitionLabel(rel), new RealizationLabel(to))
}

//class ComplexRelation extends Relation {
// TODO: implement later!
//}

class Attribute(
  // val ctype: ConceptType.Value = ConceptType.Textual,
  val dlabel: DefinitionLabel,
  val rlabel: RealizationLabel = new RealizationLabel(),
  val attrs: List[Attribute] = Nil)
  extends Concept {

  def this(attr: String) {
    this(new DefinitionLabel(attr))
  }
  def toAttrListForm = ".@" + toString

  override def toString = dlabel.toString
}

object UW {
  def getConsStr(cons: List[Constraint]): String = {
    if (cons.isEmpty) ""
    else cons.mkString("(", ",", ")")
  }
}

class UW(
  //ctype: ConceptType.Value = ConceptType.Proper,
  rl: RealizationLabel,
  val hw: String,
  val cons: List[Constraint],
  atr: List[Attribute])
  extends ElementalEntity(rl, new DefinitionLabel(hw + UW.getConsStr(cons)), atr) {

  def this(hw: String, cons: List[Constraint] = Nil, attrs: List[Attribute] = Nil) = this(new RealizationLabel(), hw, cons, attrs)

  def baseUW = dlabel.toString
}

/*
 * By specification Constraint is actually ComplexEntity, so this is slight simplification
 */
class Constraint(val rel: DefinitionLabel, val direction: String, val uw: UW) {
  def this(r: String, d: String, u: String) { this(new DefinitionLabel(r), d, new UW(u)) }
  override def toString: String = rel + direction + uw.toString
  override def equals(obj: Any): Boolean = obj.toString == toString
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