/**
 * @author Petri Kivikangas
 * @date 11.8.2012
 *
 */
package cdl.parser.test

import scala.io.Source

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

import cdl.parser.CDLParser

@RunWith(classOf[JUnitRunner])
class ArcParsingTest extends FunSpec {
  val source = getClass.getResource("/arcs.cdl")
  val data = Source.fromURL(source).getLines.toList

  describe("Parser") {
    it("should parse arc") {
      val parsed = CDLParser.parseArc(data(0))
      assert(parsed.from === "1")
      assert(parsed.relation === "agt")
      assert(parsed.to === "2")
    }
  }
}