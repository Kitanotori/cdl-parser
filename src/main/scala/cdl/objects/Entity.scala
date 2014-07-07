/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

object EntityType extends Enumeration {
  val Default, Text, Temp = Value
}

trait Entity {
  def rlabel: String
  def dlabel: String
  def dtype: DefinitionLabel.Type.Value

  def etype = dtype match {
    case DefinitionLabel.Type.Default => EntityType.Default
    case DefinitionLabel.Type.Text => EntityType.Text
    case DefinitionLabel.Type.Null if rlabel.isEmpty => EntityType.Temp
    case _ => EntityType.Default
  }

  override def toString = toString("")

  def toString(tabs: String): String = {
    etype match {
      case EntityType.Default => this.asInstanceOf[Statement].getNormalString(tabs)
      case EntityType.Text    => tabs + this.asInstanceOf[Concept].toString
      case EntityType.Temp    => this.asInstanceOf[Statement].getNormalString(tabs)
    }
  }
}


