/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.parser.test

import scala.io.Source

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

import cdl.parser.CDLParser

@RunWith(classOf[JUnitRunner])
class ConceptParsingTest extends FunSpec {
  val testData = getClass.getResource("/concepts.cdl")
  var concepts = Source.fromURL(testData, "utf-8").getLines.toList

  describe("Parsed file") {
    it("should contain the right concepts") {
      assert(concepts(0) === """<01:"test1".@attr>""")
      assert(concepts.last === """<06:16(10)>""")
      assert(concepts.size === 6)
    }
  }

  describe("Parser") {

    it("should parse 1. test concept with a quoted UW and an attribute") {
      val parsed = CDLParser.parseConcept(concepts(0))
      assert(parsed.toString === """<01:"test1".@attr>""")
      assert(parsed.rlabel === "01")
      assert(parsed.uw === """"test1"""")
      assert(parsed.attrs === List("attr"))
      assert(parsed.cons === "")
      assert(parsed.uw === """"test1".@attr""")
    }

    it("should parse 2. test concept with an unquoted UW and a list of attributes") {
      val parsed = CDLParser.parseConcept(concepts(1))
      assert(parsed.toString === "<02:test2.@attr1.@attr2>")
      assert(parsed.rlabel === "02")
      assert(parsed.uw === "test2")
      assert(parsed.attrs === List("attr1", "attr2"))
      assert(parsed.cons === "")
      assert(parsed.uw === "test2.@attr1.@attr2")
    }

    it("should parse 3. test concept with quoted empty UW without attributes") {
      val parsed = CDLParser.parseConcept(concepts(2))
      assert(parsed.toString === """<03:"">""")
      assert(parsed.rlabel === "03")
      assert(parsed.uw === """""""")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === "")
      assert(parsed.uw === """""""")
    }

    it("should parse 4. test concept with a number as UW and no attributes") {
      val parsed = CDLParser.parseConcept(concepts(3))
      assert(parsed.toString === "<04:2>")
      assert(parsed.rlabel === "04")
      assert(parsed.uw === "2")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === "")
      assert(parsed.uw === "2")
    }

    // TODO: maybe the parser should be improved to cope with syntax errors in data?
    /*it("should parse 5. test concept with empty UW and no attributes") {
      val parsed = CDLParser.parseConcept(concepts(4))
      assert(parsed.toString === "<05:>")
      assert(parsed.rlabel === "05")
      assert(parsed.uw === "")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === "")
      assert(parsed.toUWString === "")
    }

    // TODO: maybe the parser should be improved to cope with syntax errors in data?
    it("should parse 6. test concept") {
      val parsed = CDLParser.parseConcept(concepts(5))
      assert(parsed.toString === "<06:16(10)>")
      assert(parsed.rlabel === "06")
      assert(parsed.uw === "16(10)")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === "")
      assert(parsed.toUWString === "16(10)")
    }*/
  }
}