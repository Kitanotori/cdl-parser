/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.parser

import scala.collection.mutable.ListBuffer
import scala.io.{ Source, BufferedSource }
import scala.util.parsing.combinator.RegexParsers

import cdl.objects.{ UW, Statement, Entity, DefinitionLabel, Concept, CDLDocument, Arc }

class CDLParsingError(reason: String) extends Exception(reason)
class CDLParsingFailure(reason: String) extends Exception(reason)
class CDLSourceError(reason: String) extends Exception(reason)

object CDLParser {
  def parseDocument(dataSource: Any): CDLDocument = (new CDLParser(dataSource)).parseDocument
  def parseUW(dataSource: Any): UW = (new CDLParser(dataSource)).parseUW
  def parseConcept(dataSource: Any): Concept = (new CDLParser(dataSource)).parseConcept
  def parseArc(dataSource: Any): Arc = (new CDLParser(dataSource)).parseArc
}

/* @param source should be castable to BufferedSource or CharSequence */
class CDLParser(val dataSource: Any, val sourceLabel: String = "") extends RegexParsers {

  /**
   * Begins parsing from combinator stated in 'begin'.
   * Data source can be BufferedSource, CharSequence or File.
   *
   * @param begin parser combinator to begin parsing from
   * @throws CDLParsingFailure parsing failed
   * @throws CDLParsingError error while parsing
   * @return parsing result
   */
  private def doParse[T](begin: Parser[T]): ParseResult[T] = {
    if (dataSource.isInstanceOf[BufferedSource]) {
      val parsed = parseAll(begin, dataSource.asInstanceOf[BufferedSource].reader)
      dataSource.asInstanceOf[BufferedSource].close
      return parsed
    } else if (dataSource.isInstanceOf[CharSequence]) {
      parseAll(begin, dataSource.asInstanceOf[CharSequence])
    } else if (dataSource.isInstanceOf[java.io.File]) {
      parseAll(begin, Source.fromFile(dataSource.asInstanceOf[java.io.File]).reader)
    } else { throw new CDLSourceError("Invalid parsing source") }
  }

  /* Just a simple helper function */
  private def >>[T](p: ParseResult[T]): T = p match {
    case Success(parsed, _) => parsed
    case Failure(msg, rest) => throw new CDLParsingFailure(msg)
    case Error(msg, rest)   => throw new CDLParsingError(msg)
  }

  /**
   * @throws(classOf[CDLParsingFailure])
   * @throws(classOf[CDLParsingError])
   */
  def parseDocument: CDLDocument = >>(doParse(_document))

  /**
   * @throws(classOf[CDLParsingFailure])
   * @throws(classOf[CDLParsingError])
   */
  def parseConcept: Concept = >>(doParse(_textEntity))

  /**
   * @throws(classOf[CDLParsingFailure])
   * @throws(classOf[CDLParsingError])
   */
  def parseArc: Arc = >>(doParse(_arc))

  /**
   * @throws(classOf[CDLParsingFailure])
   * @throws(classOf[CDLParsingError])
   */
  def parseUW: UW = >>(doParse(_uw))

  private def _document: Parser[CDLDocument] = rep(_entity) ^^ {
    case entities => new CDLDocument(entities, sourceLabel)
  }

  private def _entity: Parser[Entity] = "{" ~> _rLabel ~ opt(_dLabel) ~ rep(_textEntity | _entity | _arc) <~ "}" ^^ {
    case rl ~ dl ~ entities => {
      val deflabel = dl match {
        case Some(dlabel) => dlabel
        case None         => DefinitionLabel.Null
      }
      var concepts = new ListBuffer[Concept]()
      var innerEntities = new ListBuffer[Entity]()
      var arcs = new ListBuffer[Arc]()
      entities.foreach(e => {
        if (e.isInstanceOf[Arc]) {
          arcs += e.asInstanceOf[Arc]
        } else if (e.isInstanceOf[Concept]) {
          concepts += e.asInstanceOf[Concept]
        } else if (e.isInstanceOf[Statement]) {
          innerEntities += e.asInstanceOf[Statement]
        } else throw new CDLParsingError("Problem parsing entities")
      })
      new Statement(rl, deflabel, (concepts ++ innerEntities).toList, arcs.toList)
    }
  }

  private def _rLabel: Parser[String] = "(\\S)*".r ^^ (_.trim)
  private def _dLabel: Parser[DefinitionLabel] = "[^<{}]*".r ^^ (DefinitionLabel.Default(_))
  private def _relation: Parser[String] = "(\\S)*".r
  private def _arc: Parser[Arc] = "[" ~> _rLabel ~ _relation ~ "[^\\]]*".r <~ "]" ^^ toArc

  private def toArc(obj: Any): Arc = obj match {
    case (l1: String) ~ (l2: String) ~ (l3: String) => new Arc(l1, l2, l3)
    case _ => throw new CDLParsingError("Arc doesn't match")
  }

  private def _textEntity: Parser[Concept] = "<" ~> ("[^:]*".r <~ ":") ~ _uw ~ _attributes <~ ">" ^^ {
    case rlabel ~ uw ~ attrs => {
      if (!uw.hw.startsWith("\"") && uw.hw.contains(".@")) {
        val x = uw.hw.split(".@")
        val h = x(0)
        val a = x.drop(1).toList
        new Concept(rlabel.trim, h, uw.constraints, a)
      } else {
        new Concept(rlabel.trim, uw.hw, uw.constraints, attrs)
      }
    }
  }

  private def _uw: Parser[UW] = _headword ~ _constraints ^^ {
    case hw ~ consts => new UW(hw, consts)
  }

  private def _headword: Parser[String] = "\"[^\"]*\"".r | "[\\S]?[^()>]*".r ^^ (_.trim)

  private def _constraints: Parser[String] = "[^\\S]*".r ~> opt("(" ~> ".*(?=\\))".r <~ "\\)[^.>]*".r) ^^ {
    case Some(cons) => cons.trim
    case None       => ""
  }

  private def _attributes: Parser[List[String]] = "[=]?[\\d.]*".r ~> rep(_attribute) ^^ {
    case Nil   => List[String]()
    case attrs => attrs
  }

  private def _attribute: Parser[String] = "[^@>]*@".r ~> "[^>.\\s]*".r
}
