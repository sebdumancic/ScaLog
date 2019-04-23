package ScalaLogic.language.lp

import ScalaLogic.language.commons.{Atom, Formula}

case class Clause(head: Atom, body: Atom*) extends Formula {

  def :^(literal: Atom): Clause = {
    Clause(head, body :+ literal: _*)
  }

  override def toString: String = s"${head.toString} :- ${body.map(_.toString).mkString(",")}"

}

object Clause {

  def apply(head: Atom, body: Atom*): Clause = new Clause(head, body: _*)
}
