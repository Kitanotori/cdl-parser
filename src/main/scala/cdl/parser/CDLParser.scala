/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.parser

import java.io.File

import scala.Array.canBuildFrom
import scala.annotation.migration
import scala.io.{ BufferedSource, Source }
import scala.util.parsing.combinator.RegexParsers

import cdl.objects.{ Attribute, CDLDocument, ComplexEntity, Concept, Constraint, DefinitionLabel, ElementalRelation, Entity, RealizationLabel, Relation, UW }

class CDLParsingError(reason: String) extends Exception(reason)
class CDLParsingFailure(reason: String) extends Exception(reason)
class CDLSourceError(reason: String) extends Exception(reason)

object CDLParser {
  def parseDocument(dataSource: Any): CDLDocument = (new CDLParser(dataSource)).parseDocument
  def parseUW(dataSource: Any): UW = (new CDLParser(dataSource)).parseUW
  def parseBaseUW(dataSource: Any): UW = (new CDLParser(dataSource)).parseBaseUW
  def parseArc(dataSource: Any): Relation = (new CDLParser(dataSource)).parseArc
  def format(doc: String): String = parseDocument(doc).toString
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
  private def doParse[T](begin: Parser[T]): ParseResult[T] = dataSource match {
    case ds: BufferedSource => parseAll(begin, ds.reader)
    case ds: CharSequence   => parseAll(begin, ds)
    case ds: File           => parseAll(begin, Source.fromFile(ds).reader)
    case _                  => throw new CDLSourceError("Invalid parsing source")
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
  def parseBaseUW: UW = >>(doParse(_baseUW))

  /**
   * @throws(classOf[CDLParsingFailure])
   * @throws(classOf[CDLParsingError])
   */
  def parseArc: Relation = >>(doParse(_arc))

  /**
   * @throws(classOf[CDLParsingFailure])
   * @throws(classOf[CDLParsingError])
   */
  def parseUW: UW = >>(doParse(_uw))

  private val ws = "(\\s)*".r
  private val noStops = "[^:<>@\\{\\}\\[\\]\\(\\)\\.\\s,]*".r

  private def _document: Parser[CDLDocument] = rep(_entity) ^^ { new CDLDocument(_) }

  private def _entity: Parser[Concept] = "{" ~> _rLabel ~ opt(_dLabel) ~ rep(_enclosedUW | _entity | _arc) <~ "}" ^^ {
    case rl ~ dl ~ entities => {
      val deflabel = dl match {
        case Some(dlabel) => dlabel
        case None         => new DefinitionLabel()
      }
      //var elemEntities: List[UW] = Nil
      //var innerEntities: List[Concept] = Nil
      var ents: List[Entity] = Nil
      var arcs: List[Relation] = Nil

      entities.foreach(_ match {
        case e: Entity   => ents :+ e
        case e: Relation => arcs :+ e
        //case e: UW            => elemEntities :+ e
        //case e: ComplexEntity => innerEntities :+ e
        case _           => throw new CDLParsingError("Problem parsing entities")
      })

      new ComplexEntity(rl, deflabel, Nil, ents, arcs)
    }
  }

  private def _rLabel: Parser[RealizationLabel] = ws ~> noStops <~ ws ^^ { new RealizationLabel(_) }

  private def _dLabel: Parser[DefinitionLabel] = ws ~> noStops <~ ws ^^ { new DefinitionLabel(_) }

  private def _enclosedUW: Parser[UW] = _uw

  private def _uw: Parser[UW] = "<" ~> _rLabel ~ ":" ~ opt(_headword) ~ opt(_constraints) ~ opt(_attributes) <~ ">" ^^ {
    case rlabel ~ mandatoryDelimiter ~ hw ~ cons ~ attrs => {
      val r = rlabel /*match {
        case Some(x) => x
        case None => new RealizationLabel()
      }*/
      var a: List[Attribute] = attrs match {
        case Some(x) => x
        case None    => Nil
      }
      val h = hw match {
        case Some(head) => {
          if (!head.startsWith("\"") && head.contains(".@")) {
            val x = head.split(".@")
            a = x.drop(1).map(a => new Attribute(a)).toList
            x(0)
          } else head
        }
        case None => ""
      }
      val c: List[Constraint] = cons match {
        case Some(x) => x
        case None    => Nil
      }
      new UW(r, h, c, a)
    }
  }

  private def _headword: Parser[String] = ws ~> ("\"[^\"]*\"".r | noStops) <~ ws

  private def _constraints: Parser[List[Constraint]] = ws ~> "(" ~> rep1sep(_constraint, ",") <~ ")" <~ ws

  private def _constraint: Parser[Constraint] = _dLabel ~ (">" | "<") ~ _baseUW ^^ {
    case dlabel ~ direction ~ cons => new Constraint(dlabel, direction, cons)
  }

  private def _attributes: Parser[List[Attribute]] = ws ~> ".@" ~> rep1sep(_attribute, ".@") <~ ws ^^ {
    case Nil   => List[Attribute]()
    case attrs => attrs
  }

  private def _attribute: Parser[Attribute] = noStops ^^ { new Attribute(_) }

  private def _arc: Parser[Relation] = "[" ~> _rLabel ~ _dLabel ~ _rLabel <~ "]" ^^ {
    case from ~ rel ~ to => new ElementalRelation(from, rel, to)
  }

  private def _baseUW: Parser[UW] = opt(_headword) ~ opt(_constraints) ~ opt(_attributes) ^^ {
    case head ~ cons ~ attrs => {
      val h = head match {
        case Some(x) => x
        case None    => ""
      }
      val c = cons match {
        case Some(x) => x
        case None    => List[Constraint]()
      }
      val a = attrs match {
        case Some(x) => x
        case None    => List[Attribute]()
      }
      new UW(h, c, a)
    }
  }
}
