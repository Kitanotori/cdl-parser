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

import cdl.newobjects.{ Attribute, CDLDocument, ComplexEntity, Concept, Constraint, DefinitionLabel, ElementalRelation, Entity, RealizationLabel, Relation, UW }

class CDLParsingError(reason: String) extends Exception(reason)
class CDLParsingFailure(reason: String) extends Exception(reason)
class CDLSourceError(reason: String) extends Exception(reason)

object CDLParser {
  def parseDocument(dataSource: Any): CDLDocument = (new CDLParser(dataSource)).parseDocument
  def parseUW(dataSource: Any): UW = (new CDLParser(dataSource)).parseUW
  def parseConcept(dataSource: Any): UW = (new CDLParser(dataSource)).parseConcept
  def parseArc(dataSource: Any): Relation = (new CDLParser(dataSource)).parseArc
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
    case ds: BufferedSource => {
      val parsed = parseAll(begin, dataSource.asInstanceOf[BufferedSource].reader)
      dataSource.asInstanceOf[BufferedSource].close
      return parsed
    }
    case ds: CharSequence => {
      parseAll(begin, dataSource.asInstanceOf[CharSequence])
    }
    case ds: File => {
      parseAll(begin, Source.fromFile(dataSource.asInstanceOf[File]).reader)
    }
    case _ => throw new CDLSourceError("Invalid parsing source")
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
  def parseConcept: UW = >>(doParse(_enclosedUW))

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

  private def _document: Parser[CDLDocument] = rep(_entity) ^^ {
    case entities => new CDLDocument(entities)
  }

  private def _entity: Parser[Concept] = "{" ~> _rLabel ~ opt(_dLabel) ~ rep(_enclosedUW | _entity | _arc) <~ "}" ^^ {
    case rl ~ dl ~ entities => {
      val deflabel = dl match {
        case Some(dlabel) => dlabel
        case None         => new DefinitionLabel()
      }
      var elemEntities: List[UW] = Nil
      var innerEntities: List[Concept] = Nil
      var arcs: List[Relation] = Nil
      var ents: List[Entity] = Nil

      entities.foreach(_ match {
        case e: Relation => arcs :+ e
        case e: Entity   => ents :+ e
        //case e: UW            => elemEntities :+ e
        //case e: ComplexEntity => innerEntities :+ e
        case _           => throw new CDLParsingError("Problem parsing entities")
      })

      new ComplexEntity(rl, deflabel, Nil, ents, arcs)
    }
  }

  private def _rLabel: Parser[RealizationLabel] = "(\\S)*".r ^^ (new RealizationLabel(_))
  private def _dLabel: Parser[DefinitionLabel] = "[^<>{}]*".r ^^ (new DefinitionLabel(_))
  private def _relation: Parser[String] = "(\\S)*".r
  private def _arc: Parser[Relation] = "[" ~> _rLabel ~ _relation ~ "[^\\]]*".r <~ "]" ^^ {
    case (from: RealizationLabel) ~ (rel: String) ~ (to: String) => new ElementalRelation(from, new DefinitionLabel(rel), new RealizationLabel(to))
    case _ => throw new CDLParsingError("Arc doesn't match")
  }

  private def _enclosedUW: Parser[UW] = "<" ~> _uw <~ ">"

  private def _uw: Parser[UW] = opt(_realizationLabel) ~ _headword ~ opt(_constraints) ~ opt(_attributes) ^^ {
    case rlabel ~ hw ~ cons ~ attrs => {
      val r = rlabel match {
        case Some(x) => x
        case None    => new RealizationLabel()
      }
      var a: List[Attribute] = Nil
      var h = ""
      if (!hw.startsWith("\"") && hw.contains(".@")) {
        val x = hw.split(".@")
        h = x(0)
        a = x.drop(1).map(a => new Attribute(a)).toList
      }
      val c = cons match {
        case Some(x) => x
        case None    => List[Constraint]()
      }
      attrs match {
        case Some(x) => a = x
      }
      new UW(r, h, c, a)
    }
  }

  private def _realizationLabel: Parser[RealizationLabel] = "[^:]*".r <~ ":" ^^ { new RealizationLabel(_) }

  private def _headword: Parser[String] = "\"[^\"]*\"".r | "[\\S]?[^()>]*".r ^^ (_.trim)

  private def _constraints: Parser[List[Constraint]] = "[^\\S]*".r ~> opt("(" ~> rep(_constraint) <~ "\\)[^.>]*".r) ^^ {
    case Some(cons) => cons
    case None       => List[Constraint]()
  }

  // ".*(?=\\))".r
  private def _constraint: Parser[Constraint] = _dLabel ~ (">" | "<") ~ _baseUW ^^ {
    case dlabel ~ direction ~ cons => new Constraint(dlabel, direction, cons)
  }

  private def _attributes: Parser[List[Attribute]] = "[=]?[\\d.]*".r ~> rep(_attribute) ^^ {
    case Nil   => List[Attribute]()
    case attrs => attrs
  }

  private def _attribute: Parser[Attribute] = "[^@>]*@".r ~> "[^>.\\s]*".r ^^ { new Attribute(_) }

  private def _baseUW: Parser[UW] = _headword ~ _constraints ^^ {
    case hw ~ cons => new UW(hw, cons)
  }
}
