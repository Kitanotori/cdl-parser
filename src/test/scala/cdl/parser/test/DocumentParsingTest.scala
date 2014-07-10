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
import cdl.objects.ComplexEntity

@RunWith(classOf[JUnitRunner])
class DocumentParsingTest extends FunSpec {

  describe("CDL document") {
    it("should parse simple document") {
      val data = getClass.getResource("/simple-document.cdl")
      val parsed: CDLDocument = CDLParser.parseDocument(Source.fromURL(data))
      val ce = parsed.entities(0).asInstanceOf[ComplexEntity]
      assert(parsed.attrs === Nil)
      assert(parsed.dlabel.isEmpty)
      assert(parsed.rlabel.isEmpty)
      assert(ce.entities.size === 2)
      assert(ce.entities(0).toString === "<2F:article(icl>document)>")
      assert(ce.entities(1).toString === "<2Q:section(icl>part).@entry>")
      assert(ce.relations(0).toString === "[2Q or 2F]")
      val asStr = parsed.toString
      val parsedString = CDLParser.parseDocument(asStr).toString
      assert(parsedString === asStr)
    }
    
    it("should parse complex document") {
      val data = getClass.getResource("/1-document.cdl")
      val parsed: CDLDocument = CDLParser.parseDocument(Source.fromURL(data))
      val ce = parsed.entities(0).asInstanceOf[ComplexEntity]
      assert(parsed.attrs === Nil)
      assert(parsed.dlabel.isEmpty)
      assert(parsed.rlabel.isEmpty)
      assert(ce.entities.size === 2)
      assert(ce.entities(0).toString === "<01:a(x s<y(h>i),k<l).@a1>")
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