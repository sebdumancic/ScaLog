package ScalaLogic.language.lp

import ScalaLogic.language.commons.{Formula, Term}


case object Cut extends Formula {

  override def substitute(subs: Map[Term, Term]): Formula = {
    this
  }

  override def toString: String = "!"

}
