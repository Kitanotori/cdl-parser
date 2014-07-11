package cdl.objects

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
  def attrs: Iterable[Attribute]
  def ctype: ConceptType.Value = ConceptType.Textual // Is not yet implemented properly anywhere

  override def toString: String = "{#"+rlabel+" "+dlabel+" }"
  override def equals(obj: Any): Boolean = obj.toString == toString
}

trait Entity extends Concept {
  override def toString: String = toString("")
  def toString(tabs: String): String // Override this!
}

class ElementalEntity(
  // val ctype: ConceptType.Value,
  val rlabel: RealizationLabel,
  val dlabel: DefinitionLabel,
  val attrs: Iterable[Attribute])
  extends Entity {

  override def toString(tabs: String): String = {
    return tabs+"<"+rlabel+":"+dlabel + Attribute.getAttrStr(attrs)+">"
  }
}

class ComplexEntity(
  // val ctype: ConceptType.Value,
  val rlabel: RealizationLabel,
  val dlabel: DefinitionLabel,
  val attrs: Iterable[Attribute] = Nil,
  val entities: Iterable[Entity] = Nil, // TODO: This should be Iterable[Entity] !
  val relations: Iterable[Relation] = Nil)
  extends Entity {

  override def toString(tabs: String): String = {
    val s = new StringBuilder(500)

    entities.foreach(e => s ++= e.toString(tabs + '\t') + '\n')

    relations.foreach(r => s ++= r.toString(tabs + '\t') + '\n')

    if (s.isEmpty) {
      tabs + '{' + rlabel + dlabel + '}'
    } else {
      tabs + '{' + rlabel + dlabel + '\n' + s.toString + tabs + '}'
    }
  }

  //def this(concepts: Iterable[Concept], arcs: Iterable[Arc]) = this("#", DefinitionLabel.Null, concepts, arcs)

  //def this(concepts: java.util.Iterable[Concept], arcs: java.util.Iterable[Arc]) = this("#", DefinitionLabel.Null, CDLConversions.toIterable(concepts), CDLConversions.toIterable(arcs))

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
  // val ctype: ConceptType.Value,
  val from: RealizationLabel,
  val relation: DefinitionLabel,
  val to: RealizationLabel,
  val rlabel: RealizationLabel = new RealizationLabel(),
  val dlabel: DefinitionLabel = new DefinitionLabel(),
  val attrs: Iterable[Attribute] = Nil)
  extends Relation {

  def this(from: String, rel: String, to: String) = this(new RealizationLabel(from), new DefinitionLabel(rel), new RealizationLabel(to))
}

//class ComplexRelation extends Relation {
// TODO: implement later!
//}

object Attribute {
  def getAttrStr(attr: Iterable[Attribute]): String = {
    if (attr.isEmpty) ""
    else attr.mkString(".@", ".@", "")
  }
}

class Attribute(
  // val ctype: ConceptType.Value = ConceptType.Textual,
  val dlabel: DefinitionLabel,
  val rlabel: RealizationLabel = new RealizationLabel(),
  val attrs: Iterable[Attribute] = Nil)
  extends Concept {

  def this(attr: String) {
    this(new DefinitionLabel(attr))
  }
  def toAttrIterableForm = ".@"+toString

  override def toString = dlabel.toString
}

class UW(
  //ctype: ConceptType.Value = ConceptType.Proper,
  rl: RealizationLabel,
  val hw: String,
  val cons: Iterable[Constraint],
  atr: Iterable[Attribute])
  extends ElementalEntity(rl, new DefinitionLabel(hw + Constraint.getConsStr(cons)), atr) {

  def this(hw: String, cons: Iterable[Constraint] = Nil, attrs: Iterable[Attribute] = Nil) = this(new RealizationLabel(), hw, cons, attrs)

  def this(rlab: String, head: String, const: Iterable[Constraint]) = this(new RealizationLabel(rlab), head, const, Nil)

  def baseUW: String = dlabel.toString

  def getConstStr: String = Constraint.getConsStr(cons)

  def getAttrStr: String = Attribute.getAttrStr(attrs)
}

object Constraint {
  def getConsStr(cons: Iterable[Constraint]): String = {
    if (cons.isEmpty) ""
    else cons.mkString("(", ",", ")")
  }
}

/*
 * By specification Constraint is actually ComplexEntity, so this is slight simplification
 */
class Constraint(val rel: DefinitionLabel, val direction: String, val uw: UW) {
  def this(r: String, d: String, u: String) { this(new DefinitionLabel(r), d, new UW(u)) }
  def this(r: String, d: String, u: UW) { this(new DefinitionLabel(r), d, u) }
  override def toString: String = rel + direction + uw.baseUW
  override def equals(obj: Any): Boolean = obj.toString == toString
}

class CDLDocument(
  ent: Iterable[Entity] = Nil,
  rl: RealizationLabel = new RealizationLabel(),
  dl: DefinitionLabel = new DefinitionLabel(),
  atr: Iterable[Attribute] = Nil,
  rel: Iterable[Relation] = Nil) extends ComplexEntity(rl, dl, atr, ent, rel) {

  // def this(entities: java.util.Iterable[Entity], title: String) = this(CDLConversions.toIterable(entities), title)

  override def toString = {
    val res = new StringBuilder(400000)
    entities.map(e => e.toString).addString(res, "\n")
    res.toString
  }
}