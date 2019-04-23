package ScalaLogic.solvers

import ScalaLogic.language.commons.{Atom, Constant, Term, Variable}

abstract class Solver() {

  def hasSolution(queryAtom: Atom): Boolean

  def oneSolution(queryAtom: Atom): Map[Variable, Constant]

  def allSolutions(queryAtom: Atom): Seq[Map[Variable, Constant]]

}
