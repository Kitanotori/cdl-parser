/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.parser.test

import scala.io.Source
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import cdl.newobjects.CDLDocument
import cdl.parser.CDLParser
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DocumentParsingTest extends FunSpec {

  describe("CDL document") {
    it("should parse 1. document") {
      val data = getClass.getResource("/1-document.cdl")
      println("Starting to parse")
      val parsed: CDLDocument = CDLParser.parseDocument(Source.fromURL(data))
      println("Finished parsing")
      assert(parsed.attrs === Nil)
      assert(parsed.dlabel === Nil)
      assert(parsed.rlabel === Nil)
      val asStr = parsed.toString
      assert(CDLParser.parseDocument(asStr).toString === asStr)
    }
  }
}