/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

import cdl.javacompat.CDLConversions

class CDLDocument(val entities: List[Entity] = Nil, var title: String = "") {

  def this(entities: java.util.List[Entity], title: String) = this(CDLConversions.toList(entities), title)

  override def toString = {
    val res = new StringBuilder(400000)
    entities.map(e => e.toString).addString(res, "\n")
    res.toString
  }
}