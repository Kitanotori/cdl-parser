/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

object DefinitionLabel {
  object Type extends Enumeration {
    val Default, Text, Null = Value
  }
  val Null = new DefinitionLabel("", Type.Null)
  def Text(t: String) = new DefinitionLabel(t, Type.Text)
  def Default(s: String) = new DefinitionLabel(s, Type.Default)
}

class DefinitionLabel(val dlabel: String, val dtype: DefinitionLabel.Type.Value) {
  override def toString: String = dlabel
}