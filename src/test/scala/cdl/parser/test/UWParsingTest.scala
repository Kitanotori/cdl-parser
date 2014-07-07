/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.parser.test

import scala.io.Source

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec

import cdl.parser.CDLParser

@RunWith(classOf[JUnitRunner])
class UWParsingTest extends FunSpec {
  val testData = getClass.getResource("/uwstrings.cdl")
  var uws = Source.fromURL(testData, "utf-8").getLines.toList

  describe("parsed file") {
    it("should contain the right UWs") {
      assert(uws(0) === "test0")
      assert(uws.last === ">1.@topic")
    }
  }

  describe("plain UWs:") {
    it("test0 should parse plain UW") {
      val parsed = CDLParser.parseUW(uws(0))
      assert(parsed.hw === "test0")
      assert(parsed.cons === "")
      assert(parsed.toString === "test0")
    }
    it("test2 should parse quoted UW") {
      val parsed = CDLParser.parseUW(uws(2))
      assert(parsed.hw === "\"test2\"")
      assert(parsed.cons === "")
      assert(parsed.toString === "\"test2\"")
    }
    it("test4 should parse UW with tailing dots") {
      val parsed = CDLParser.parseUW(uws(4))
      assert(parsed.hw === "test4..")
      assert(parsed.cons === "")
      assert(parsed.toString === "test4..")
    }
  }

  describe("UWs with constraints:") {
    it("test8 should parse UW with constraints") {
      val parsed = CDLParser.parseUW(uws(8))
      assert(parsed.hw === "test8")
      assert(parsed.cons.toString === "icl>uw")
      assert(parsed.toString === "test8(icl>uw)")
    }
    it("test11 should parse UW with nested constraints") {
      val parsed = CDLParser.parseUW(uws(11))
      assert(parsed.hw === "test11")
      assert(parsed.cons.toString === "icl>uw(agt>uw2,obj>uw3),icl>uw4(agt>uw5)")
      assert(parsed.toString === "test11(icl>uw(agt>uw2,obj>uw3),icl>uw4(agt>uw5))")
    }
  }

  describe("UWs with constraints written using shorthand") {
    it("test13 should parse UW with shorthand notation constraints") {
      val parsed = CDLParser.parseUW(uws(13))
      assert(parsed.hw === "test13")
      assert(parsed.cons === "agt>thing,obj>role>effect")
      assert(parsed.toString === "test13(agt>thing,obj>role>effect)")
    }
  }

  // TODO: maybe the parser should be improved to cope with syntax errors in data?
  /*describe("UWs with syntax errors") {
    it("test14 should turn < to >") {
      val parsed = CDLParser.parseUW(uws(14))
      assert(parsed.hw === "test14")
      assert(parsed.cons === "icl>uw")
      assert(parsed.toString === "test14(icl>uw)")
    }
  }*/
}