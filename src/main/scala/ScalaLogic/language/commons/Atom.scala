package ScalaLogic.language.commons

import ScalaLogic.language.lp.Clause

case class Atom(predicate: Predicate, arguments: SimpleTerm*) extends Formula with WithArguments {

  def getPredicate: Predicate = predicate

  def isGround: Boolean = getArguments.forall(_.isInstanceOf[Constant])


  def vars: Seq[Variable] = arguments.collect { case x: Variable => x }


  def :-(body: Atom*): Clause = Clause(this, body: _*)


  def :-(body: Atom): Clause = Clause(this, Seq(body): _*)


  override def toString: String = s"${predicate.getName}(${getArguments.map(_.toString).mkString(",")})"

}

object Atom {

  def apply(predicate: Predicate, args: SimpleTerm*): Atom =
    new Atom(predicate, args: _*)

}
