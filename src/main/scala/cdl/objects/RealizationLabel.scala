/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

class RealizationLabel(var label: String = "") {
  label = label.trim
  if (label == "#") label = ""

  def isEmpty = label.isEmpty
  def nonEmpty = label.nonEmpty

  override def toString: String =
    if (label != null) label
    else "NULL"
}