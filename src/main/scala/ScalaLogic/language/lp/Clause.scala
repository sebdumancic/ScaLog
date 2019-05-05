package ScalaLogic.language.lp

import ScalaLogic.language.commons.{Atom, Formula, Term}


case class Clause(heads: Seq[Atom], body: Seq[Atom]) extends Formula {

  def :^(literal: Atom): Clause = {
    Clause(heads, body :+ literal)
  }

  override def substitute(subs: Map[Term, Term]): Formula = {
    Clause(heads.map(_.substitute(subs)), body.map(_.substitute(subs)))
  }

  def isHorn: Boolean = {
    heads.lengthCompare(1) == 0
  }

  override def toString: String = s"${heads.mkString(";")} :- ${body.map(_.toString).mkString(",")}"

}


case class HornClause(head: Atom, override val body: Seq[Atom]) extends Clause(Seq(head), body)

