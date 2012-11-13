/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

object EntityTypes {
  object EType extends Enumeration {
    val Default, Text, Temp = Value
  }
}

trait Entity {
  import EntityTypes.EType
  def rlabel: String
  def dlabel: String
  def dtype: DefinitionLabel.Type.Value

  def etype = dtype match {
    case DefinitionLabel.Type.Default => EType.Default
    case DefinitionLabel.Type.Text => EType.Text
    case DefinitionLabel.Type.Null if rlabel.isEmpty => EType.Temp
    case _ => EType.Default
  }

  override def toString = toString("")

  def toString(tabs: String): String = {
    etype match {
      case EType.Default => this.asInstanceOf[Statement].getNormalString(tabs)
      case EType.Text    => tabs + this.asInstanceOf[Concept].toString
      case EType.Temp    => this.asInstanceOf[Statement].getNormalString(tabs)
    }
  }
}


