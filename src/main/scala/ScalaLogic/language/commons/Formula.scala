package ScalaLogic.language.commons

import ScalaLogic.language.lp.HornClause

abstract class Formula {

  def substitute(subs: Map[Term, Term]): Formula

}



/**
  *
  *         ATOM
  *
  *
  * */

case class Atom(predicate: Predicate, arguments: Term*) extends Formula with WithArguments {

  def getPredicate: Predicate = predicate

  override def substitute(subs: Map[Term, Term]): Atom = {
    Atom(predicate, _substitute(subs): _*)
  }

  def :-(body: Atom*): HornClause = HornClause(this, body)


  def :-(body: Atom): HornClause = HornClause(this, Seq(body))


  override def toString: String = s"${predicate.getName}(${getArguments.map(_.toString).mkString(",")})"

}


/**
  *
  *       NEGATION
  *
  * */

case class Not(formula: Formula) extends Formula {

  override def substitute(subs: Map[Term, Term]): Not = {
    Not(formula.substitute(subs))
  }

}




abstract class BinaryOp(left: Formula, token: BinToken, right: Formula) extends Formula {

  def isAssociative: Boolean = {
    token.isAssociative
  }

  def isCommutative: Boolean = {
    token.isCommutative
  }

  protected def _equal(o: BinaryOp): Boolean = {
    token == o.token && ((left == o.left && right == o.right) ||
                         (token.isCommutative && right == o.left && left == o.right))
  }

  override def equals(obj: Any): Boolean = obj match {
    case t: BinaryOp => _equal(t)
    case _ => false
  }

}





