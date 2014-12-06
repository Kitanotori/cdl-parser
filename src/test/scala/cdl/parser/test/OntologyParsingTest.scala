/**
 * @author Petri Kivikangas
 * @date 12.11.2012
 *
 */
package cdl.parser.test

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import cdl.parser.OntologyParser

@RunWith(classOf[JUnitRunner])
class OntologyParsingTest extends FunSpec {
  val testData = getClass.getResource("/UNLOntology_tree.txt")

  describe("OntologyParser") {
    it("should parse ontology") {
      val ontoTree = OntologyParser.parse(testData.getPath)
      assert(ontoTree != null)
      assert(ontoTree.uw === "uw")
      assert(ontoTree.child(0).uw === "adverbial concept")
      assert(ontoTree.child(0).child(0).uw === "how")
    }
  }
}