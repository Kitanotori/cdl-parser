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

import cdl.objects.CDLDocument
import cdl.parser.CDLParser

@RunWith(classOf[JUnitRunner])
class DocumentParsingTest extends FunSpec {

  describe("CDL document") {
    it("should parse simple document") {
      val data = getClass.getResource("/simple-document.cdl")
      val parsed: CDLDocument = CDLParser.parseDocument(Source.fromURL(data))
      assert(parsed.attrs === Nil)
      assert(parsed.dlabel.isEmpty)
      assert(parsed.rlabel.isEmpty)
      val asStr = parsed.toString
      println(asStr)
      val parsedString = CDLParser.parseDocument(asStr).toString
      println("1.\n"+asStr)
      println("\n2.\n"+parsedString)
      assert(parsedString.replaceAll("\\s", "") === asStr.replaceAll("\\s", ""))
    }
    
    it("should parse complex document") {
      val data = getClass.getResource("/1-document.cdl")
      val parsed: CDLDocument = CDLParser.parseDocument(Source.fromURL(data))
      assert(parsed.attrs === Nil)
      assert(parsed.dlabel.isEmpty)
      assert(parsed.rlabel.isEmpty)
      val asStr = parsed.toString
      assert(CDLParser.parseDocument(asStr).toString.replaceAll("\\s", "") === asStr.replaceAll("\\s", ""))
    }
    
    it("should parse complex and longer document") {
      val data = getClass.getResource("/3-document.cdl")
      val parsed: CDLDocument = CDLParser.parseDocument(Source.fromURL(data))
      assert(parsed.attrs === Nil)
      assert(parsed.dlabel.isEmpty)
      assert(parsed.rlabel.isEmpty)
      val asStr = parsed.toString
      val parsedString = CDLParser.parseDocument(asStr).toString
      println("1.\n"+asStr)
      println("\n2.\n"+parsedString)
      assert(parsedString.replaceAll("\\s", "") === asStr.replaceAll("\\s", ""))
    }
  }
}