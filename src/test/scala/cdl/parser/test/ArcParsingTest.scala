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

import cdl.objects.{ DefinitionLabel, ElementalRelation, RealizationLabel, Relation }
import cdl.parser.CDLParser

@RunWith(classOf[JUnitRunner])
class ArcParsingTest extends FunSpec {
  val source = Source.fromURL(getClass.getResource("/arcs.cdl"), "utf-8")
  val data = source.getLines.toList
  source.close()

  describe("Parser") {
    it("should parse arc") {
      val parsed = CDLParser.parseArc(data(0))
      assert(parsed.from.toString === "1")
      assert(parsed.relation.toString === "agt")
      assert(parsed.to.toString === "2")
    }

    it("should output arc as valid string") {
      val arc: Relation = new ElementalRelation(new RealizationLabel("1"), new DefinitionLabel("agt"), new RealizationLabel("2"))
      assert(arc.from === "1")
      assert(arc.to === "2")
      assert(arc.relation === "agt")
      assert(arc.toString === "[1 agt 2]")
    }
  }
}