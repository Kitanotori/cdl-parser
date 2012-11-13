/**
 * @author Petri Kivikangas
 * @date 6.2.2012
 *
 */
package cdl.objects

class Constraint(val rel: String, val uw: UW) { override def toString: String = rel + '>' + uw.toString }