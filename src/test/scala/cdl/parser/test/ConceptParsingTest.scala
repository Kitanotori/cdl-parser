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
import cdl.objects.Attribute
import cdl.objects.Constraint
import cdl.objects.UW

@RunWith(classOf[JUnitRunner])
class ConceptParsingTest extends FunSpec {
  val source = Source.fromURL(getClass.getResource("/concepts.cdl"), "utf-8")
  var concepts = source.getLines.toList
  source.close()

  describe("Parsed file") {
    it("should read correct data from test file") {
      assert(concepts(0) === """<01:"test1".@attr>""")
      assert(concepts.last === """<16:etc.(a>b).@c>""")
      assert(concepts.size === 16)
    }
  }

  describe("Parser") {
    it("should parse 1. test concept with a quoted UW and an attribute") {
      val parsed = CDLParser.parseUW(concepts(0))
      assert(parsed.toString() === """<01:"test1".@attr>""")
      assert(parsed.rlabel.toString === "01")
      assert(parsed.hw === "\"test1\"")
      assert(parsed.attrs === List(new Attribute("attr")))
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "\"test1\"")
    }

    it("should parse 2. test concept with an unquoted UW and a list of attributes") {
      val parsed = CDLParser.parseUW(concepts(1))
      assert(parsed.toString() === "<02:test2.@attr1.@attr2>")
      assert(parsed.rlabel.toString === "02")
      assert(parsed.hw === "test2")
      assert(parsed.attrs === List("attr1", "attr2"))
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "test2")
    }

    it("should parse 3. test concept with quoted empty UW without attributes") {
      val parsed = CDLParser.parseUW(concepts(2))
      assert(parsed.toString() === """<03:"">""")
      assert(parsed.rlabel.toString === "03")
      assert(parsed.hw === "\"\"")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "\"\"")
    }

    it("should parse 4. test concept with a number as UW and no attributes") {
      val parsed = CDLParser.parseUW(concepts(3))
      assert(parsed.toString() === "<04:2>")
      assert(parsed.rlabel.toString === "04")
      assert(parsed.hw === "2")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "2")
    }

    it("should parse 5. test concept with empty UW and no attributes") {
      val parsed = CDLParser.parseUW(concepts(4))
      assert(parsed.toString() === "<05:>")
      assert(parsed.rlabel.toString === "05")
      assert(parsed.hw === "")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "")
    }

    it("should parse 6. test concept") {
      val parsed = CDLParser.parseUW(concepts(5))
      assert(parsed.toString() === "<06:16(x>y)>")
      assert(parsed.rlabel.toString === "06")
      assert(parsed.hw === "16")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === List(new Constraint("x", ">", "y")))
      assert(parsed.baseUW === "16(x>y)")
    }

    it("should parse 7. test concept") {
      val parsed = CDLParser.parseUW(concepts(6))
      assert(parsed.toString() === "<07:16(x>y).@c>")
      assert(parsed.rlabel.toString === "07")
      assert(parsed.hw === "16")
      assert(parsed.attrs === List("c"))
      assert(parsed.cons === List(new Constraint("x", ">", "y")))
      assert(parsed.baseUW === "16(x>y)")
    }

    it("should parse 8. test concept") {
      val parsed = CDLParser.parseUW(concepts(7))
      assert(parsed.toString() === "<08:16(x>y).@c.@d>")
      assert(parsed.rlabel.toString === "08")
      assert(parsed.hw === "16")
      assert(parsed.attrs === List("c", "d"))
      assert(parsed.cons === List(new Constraint("x", ">", "y")))
      assert(parsed.baseUW === "16(x>y)")
    }

    it("should parse 9. test concept") {
      val parsed = CDLParser.parseUW(concepts(8))
      assert(parsed.toString() === "<09:a(b>c(d>e,f<g)).@h.@i>")
      assert(parsed.rlabel.toString === "09")
      assert(parsed.hw === "a")
      assert(parsed.attrs === List("h", "i"))
      assert(parsed.cons === List(new Constraint("b", ">",
        new UW("c", List(new Constraint("d", ">", "e"), new Constraint("f", "<", "g"))))))
      assert(parsed.baseUW === "a(b>c(d>e,f<g))")

    }

    it("should parse 10. test concept") {
      val parsed = CDLParser.parseUW(concepts(9))
      assert(parsed.toString() === "<10:a b.@c>")
      assert(parsed.rlabel.toString === "10")
      assert(parsed.hw === "a b")
      assert(parsed.attrs === List("c"))
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "a b")
    }

    it("should parse 11. test concept") {
      val parsed = CDLParser.parseUW(concepts(10))
      assert(parsed.toString() === "<11:3.0>")
      assert(parsed.rlabel.toString === "11")
      assert(parsed.hw === "3.0")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "3.0")
    }

    it("should parse 12. test concept") {
      val parsed = CDLParser.parseUW(concepts(11))
      assert(parsed.toString() === "<12:3.0.@a>")
      assert(parsed.rlabel.toString === "12")
      assert(parsed.hw === "3.0")
      assert(parsed.attrs === List("a"))
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "3.0")
    }

    it("should parse 13. test concept") {
      val parsed = CDLParser.parseUW(concepts(12))
      assert(parsed.toString() === "<13:3.0(a>b).@c>")
      assert(parsed.rlabel.toString === "13")
      assert(parsed.hw === "3.0")
      assert(parsed.attrs === List("c"))
      assert(parsed.cons === List(new Constraint("a", ">", "b")))
      assert(parsed.baseUW === "3.0(a>b)")
    }

    it("should parse 14. test concept") {
      val parsed = CDLParser.parseUW(concepts(13))
      assert(parsed.toString() === "<14:etc.>")
      assert(parsed.rlabel.toString === "14")
      assert(parsed.hw === "etc.")
      assert(parsed.attrs === Nil)
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "etc.")
    }

    it("should parse 15. test concept") {
      val parsed = CDLParser.parseUW(concepts(14))
      assert(parsed.toString() === "<15:etc..@a>")
      assert(parsed.rlabel.toString === "15")
      assert(parsed.hw === "etc.")
      assert(parsed.attrs === List("a"))
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "etc.")
    }

    it("should parse 16. test concept") {
      val parsed = CDLParser.parseUW(concepts(15))
      assert(parsed.toString() === "<16:etc.(a>b).@c>")
      assert(parsed.rlabel.toString === "16")
      assert(parsed.hw === "etc.")
      assert(parsed.attrs === List("c"))
      assert(parsed.cons === List(new Constraint("a", ">", "b")))
      assert(parsed.baseUW === "etc.(a>b)")
    }

  }
}