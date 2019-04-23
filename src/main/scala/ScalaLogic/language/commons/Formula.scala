package ScalaLogic.language.commons

import ScalaLogic.language.lp.Clause

abstract class Formula {

}



/**
  *
  *         ATOM
  *
  *
  * */

case class Atom(predicate: Predicate, arguments: Term*) extends Formula with WithArguments {

  def getPredicate: Predicate = predicate

  def :-(body: Atom*): Clause = Clause(this, body: _*)


  def :-(body: Atom): Clause = Clause(this, Seq(body): _*)


  override def toString: String = s"${predicate.getName}(${getArguments.map(_.toString).mkString(",")})"

}


