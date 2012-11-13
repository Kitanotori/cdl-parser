/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

import scala.collection.JavaConversions

class Concept(var rlabel: String, hw: String, consts: String,
              var attributes: List[String]) extends UW(hw, consts) with Entity {

  def this(r: String = "", h: String = "", c: String = "") = this(r, h, c, Nil)

  /**
   * Java convenience method for adding attributes
   */
  def addAttrs(newAttrs: java.util.List[String]) = {
    attributes = attributes ::: JavaConversions.asScalaBuffer(newAttrs).toList
  }

  override def toString: String = '<' + rlabel + ':' + toUWString + '>'

  def toUWString = dlabel + attributes.map(a => ".@"+a).mkString

}