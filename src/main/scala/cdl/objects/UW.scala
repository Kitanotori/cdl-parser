/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

object UW {
  def toUWString(hw: String, constraints: String): String = {
    val str = new StringBuilder(hw)
    if (constraints.nonEmpty) str ++= '(' + constraints + ')'
    return str.toString
  }
}

/**
 * According to UNL 2003 specification (http://www.undl.org/unlsys/uw/uwmanv20.htm),
 * the syntax for UWs is defined as follows:
 *
 * 	<UW>				::= <Headword> [<Constraint List>]
 * 	<Headword>			::= <character>'
 * 	<Constraint List>	::= '('<Constraint> [ ',' <Constraint>]... ')'
 * 	<Constraint>		::= <Relation Label> { '>' | '<' } <Headword> [<Constraint List> | <Constraint without relation>]
 * 	<Constraint without
 * relation> 			::= { '>' | '<Headword>' }...
 * 	<Relation Label>	::= 'agt' | 'and' | ... | 'via' | 'equ' | 'icl' | 'iof'
 * 	<character>			::= 'A' | ... | 'Z' | 'a' | ... | 'z' | 0 | 1 | 2 | ... | 9 | '_' | ' ' | '#' | '!' | '$' | '%' | '=' | '^' | '~' | '|' | '@' | '+' | '-' | '<' | '>' | '?'
 *
 * 	where:
 * 	                < >		variable
 * 	                " "		terminal symbol
 * 	                ::=		... is defined as ...
 * 	                |		disjunction ("or")
 * 	                [ ]		optional element
 * 	                { }		alternative element
 * 	                ...		to be repeated more than 0 times
 * 	                ^		negation ("not")
 *
 *
 *
 */
class UW(val hw: String, var constraints: String = "")
  extends DefinitionLabel(UW.toUWString(hw, constraints), DefinitionLabel.Type.Text) {

  def this(hw: String) = this(hw, "")

  def uw = dlabel

}