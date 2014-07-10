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

import cdl.objects.Constraint

@RunWith(classOf[JUnitRunner])
class UWParsingTest extends FunSpec {
  val testData = getClass.getResource("/uwstrings.cdl")
  var uws = Source.fromURL(testData, "utf-8").getLines.toList

  describe("Parsed file") {
    it("should contain the right UWs") {
      assert(uws(0) === "test0")
      assert(uws.last === "test20(icl\t<uw)")
    }
  }

  describe("Should parse constrained UW") {
    it("with attributes") {
      val parsed = CDLParser.parseBaseUW(uws(4))
      assert(parsed.hw === "test4")
      assert(parsed.cons === List(new Constraint("x", ">", "y")))
      assert(parsed.attrs === List("a"))
      assert(parsed.baseUW === "test4(x>y)")
    }
  }

  describe("Should parse plain UW") {
    it("as such") {
      val parsed = CDLParser.parseBaseUW(uws(0))
      assert(parsed.hw === "test0")
      assert(parsed.cons === Nil)
      assert(parsed.attrs === Nil)
      assert(parsed.baseUW === "test0")
    }

    it("with an attribute") {
      val parsed = CDLParser.parseBaseUW(uws(1))
      assert(parsed.hw === "test1")
      assert(parsed.cons === Nil)
      assert(parsed.attrs === List("a"))
      assert(parsed.baseUW === "test1")
    }

    it("with attributes") {
      val parsed = CDLParser.parseBaseUW(uws(2))
      assert(parsed.hw === "test2")
      assert(parsed.cons === Nil)
      assert(parsed.attrs === List("a", "b"))
      assert(parsed.baseUW === "test2")
    }
  }

  describe("UWs with constraints:") {
    it("should parse UW with constraints") {
      val parsed = CDLParser.parseBaseUW(uws(3))
      assert(parsed.hw === "test3")
      assert(parsed.cons === List(new Constraint("x", ">", "y")))
      assert(parsed.baseUW === "test3(x>y)")
    }
  }
}