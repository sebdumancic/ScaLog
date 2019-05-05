package ScalaLogic.language.fol

import ScalaLogic.language.commons.{AND, BinaryOp, EQV, Formula, IMPL, OR, Term, Variable}


case class Conjunction(left: Formula, right: Formula) extends BinaryOp(left, AND, right) {

  override def substitute(subs: Map[Term, Term]): Conjunction = {
    Conjunction(left.substitute(subs), right.substitute(subs))
  }
}


case class Disjunction(left: Formula, right: Formula) extends BinaryOp(left, OR, right) {

  override def substitute(subs: Map[Term, Term]): Formula = {
    Disjunction(left.substitute(subs), right.substitute(subs))
  }
}




case class Implication(left: Formula, right: Formula) extends BinaryOp(left, IMPL, right) {

  override def substitute(subs: Map[Term, Term]): Formula = {
    Implication(left.substitute(subs), right.substitute(subs))
  }
}





case class Equivalence(left: Formula, right: Formula) extends BinaryOp(left, EQV, right) {

  override def substitute(subs: Map[Term, Term]): Equivalence = {
    Equivalence(left.substitute(subs), left.substitute(subs))
  }
}




abstract class QuFormula(quToken: QuToken, variable: Variable, formula: Formula) extends Formula {

  def isUniversal: Boolean = quToken.isUniversal

  def isExistential: Boolean = quToken.isExistential
}


case class Forall(variable: Variable, formula: Formula) extends QuFormula(FORALL, variable, formula) {

  override def substitute(subs: Map[Term, Term]): Formula = {
    Forall(variable, formula.substitute(subs))
  }

}

case class Exists(variable: Variable, formula: Formula) extends QuFormula(EXISTS, variable, formula) {

  override def substitute(subs: Map[Term, Term]): Formula = {
    Exists(variable, formula.substitute(subs))
  }
}