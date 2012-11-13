/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

class Arc(var from: String, val relation: String, var to: String) {
  override def toString = toString("")
  def toString(tabs: String) = tabs+"["+from+" "+relation+" "+to+"]"
  def toCypherString = "x"+from+"-[:"+relation+"]->x"+to
}