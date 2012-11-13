/**
 * @author Petri Kivikangas
 * @date 28.4.2012
 *
 */
package cdl.javacompat

import scala.collection.JavaConversions

object CDLConversions {
  def toList[T](coll: java.util.List[T]): List[T] = JavaConversions.asScalaBuffer(coll).toList
}