/**
 * @author Petri Kivikangas
 * @date 11.4.2012
 *
 */
package cdl.parser

import java.io.File

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.xml.Node

import org.slf4j.LoggerFactory

object OntologyParser extends RegexParsers {
  private val logger = LoggerFactory.getLogger(this.getClass)
  private var uws = 0

  override def skipWhitespace = false

  def parse(path: String): UwNode = {
    uws = 0
    logger.debug("Parsing ontologies from "+path+"...")
    val time = System.currentTimeMillis
    val f: File = new File(path)
    parseAll(uw(-1), Source.fromFile(f).reader) match {
      case Success(parsed, _) => {
        logger.debug("Parsed "+uws+" UWs in "+(System.currentTimeMillis - time) / 1000.0+" seconds"); parsed
      }
      case Failure(msg, rest) => {
        logger.error("FAILURE parsing UW ontology: "+msg); null
      }
      case Error(msg, rest) => {
        logger.error("ERROR parsing UW ontology: "+msg); null
      }
    }

  }

  private def uw(tabs: Int): Parser[UwNode] = tabbing(tabs) ~ "[^\n]+".r <~ "\n" >> {
    case tabs ~ word => rep(uw(tabs)) ^^ { uws += 1; new UwNode(word.trim, _) }
  }

  def tabbing(min: Int) = "\\t*".r ^^ { _.count(_ == '\t') } ^? {
    case i if i > min => i
  }

  class UwNode(val uw: String, override val child: List[UwNode]) extends Node {
    def label = uw
    override def text = uw

    private def toStr(tab: Int): String = {
      val t = "\t" * tab
      if (child.isEmpty) t+"</"+label+">\n"
      else {
        val sb = new StringBuilder(300000, t + '<' + label+">\n")
        child.map(c => c.toStr(tab + 1)).addString(sb)
        sb ++= t+"</"+label+">\n"
        sb.toString
      }
    }

    override def toString: String = toStr(0)
  }
}