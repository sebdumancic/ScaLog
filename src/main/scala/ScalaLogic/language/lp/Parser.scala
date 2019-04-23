package ScalaLogic.language.lp

import ScalaLogic.language.commons.{Atom, Constant, Formula, Predicate, Term}

import scala.util.matching.Regex
import scala.util.parsing.combinator._

class Parser(val predicates: Set[Predicate]) extends RegexParsers {
  val predicateMap: collection.mutable.Map[String, Predicate] = collection.mutable.Map()

  predicates.foreach(p => predicateMap(p.getSignature) = p)

  val con_symbol: Regex = """[a-z]+[a-zA-Z0-9_]*""".r
  val var_symbol: Regex = """[A-Z]+[a-zA-Z0-9_]*""".r
  val pred_symbol: Regex = """[a-zA-Z0-9_]+""".r


  def term: Parser[Term] = con_symbol ^^ { x => Constant(x)}



}
