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

import cdl.objects.{Statement, Concept}
import cdl.parser.CDLParser

@RunWith(classOf[JUnitRunner])
class DocumentParsingTest extends FunSpec {

  describe("CDL document") {
    it("should parse 1. document") {
      val data = getClass.getResource("/1-document.cdl")
      val parsed = CDLParser.parseDocument(Source.fromURL(data))
      val stat = parsed.entities(0).asInstanceOf[Statement]
      val conc = stat.entities(0).asInstanceOf[Concept]
      assert(conc.hw === "merge")
      assert(CDLParser.parseDocument(parsed.toString).toString === parsed.toString)
    }
    it("should parse 2. document") {
      val data = getClass.getResource("/2-document.cdl")
      val parsed = CDLParser.parseDocument(Source.fromURL(data))
      val stat = parsed.entities(0).asInstanceOf[Statement]
      val conc = stat.entities(0).asInstanceOf[Concept]
      assert(conc.hw === "merge")
      assert(CDLParser.parseDocument(parsed.toString).toString === parsed.toString)
    }
    it("should parse 3. document") {
      val data = getClass.getResource("/3-document.cdl")
      val parsed = CDLParser.parseDocument(Source.fromURL(data))
      val stat = parsed.entities(0).asInstanceOf[Statement]
      val conc = stat.entities(0).asInstanceOf[Concept]
      assert(CDLParser.parseDocument(parsed.toString).toString === parsed.toString)
    }
  }
}