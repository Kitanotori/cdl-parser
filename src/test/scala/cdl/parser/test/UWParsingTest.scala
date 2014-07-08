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
      assert(uws.last === ">1.@topic")
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

    it("with attribute") {
      val parsed = CDLParser.parseBaseUW(uws(1))
      assert(parsed.hw === "test1")
      assert(parsed.cons === Nil)
      assert(parsed.attrs === List("a"))
      assert(parsed.baseUW === "test1")
    }

    it("as quoted UW") {
      val parsed = CDLParser.parseBaseUW(uws(2))
      assert(parsed.hw === "\"test2\"")
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "\"test2\"")
    }
    it("with tailing dots") {
      val parsed = CDLParser.parseBaseUW(uws(4))
      assert(parsed.hw === "test4..")
      assert(parsed.cons === Nil)
      assert(parsed.baseUW === "test4..")
    }
  }

  describe("UWs with constraints:") {
    it("should parse UW with constraints") {
      val parsed = CDLParser.parseBaseUW(uws(8))
      assert(parsed.hw === "test8")
      assert(parsed.cons.toString === "icl>uw")
      assert(parsed.baseUW === "test8(icl>uw)")
    }
    it("should parse UW with nested constraints") {
      val parsed = CDLParser.parseBaseUW(uws(11))
      assert(parsed.hw === "test11")
      assert(parsed.cons.toString === "icl>uw(agt>uw2,obj>uw3),icl>uw4(agt>uw5)")
      assert(parsed.baseUW === "test11(icl>uw(agt>uw2,obj>uw3),icl>uw4(agt>uw5))")
    }
  }

  describe("UWs with constraints written using shorthand") {
    it("should parse UW with shorthand notation constraints") {
      val parsed = CDLParser.parseBaseUW(uws(13))
      assert(parsed.hw === "test13")
      assert(parsed.cons === "agt>thing,obj>role>effect")
      assert(parsed.baseUW === "test13(agt>thing,obj>role>effect)")
    }
  }

  // TODO: maybe the parser should be improved to cope with syntax errors in data?
  describe("UWs with syntax errors") {
    it("test14 should turn < to >") {
      val parsed = CDLParser.parseBaseUW(uws(14))
      assert(parsed.hw === "test14")
      assert(parsed.cons === "icl>uw")
      assert(parsed.baseUW === "test14(icl>uw)")
    }
  }
}