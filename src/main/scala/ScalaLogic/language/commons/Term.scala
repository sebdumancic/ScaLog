package ScalaLogic.language.commons

/**
  * Implements the basic Term functionality, base class for Constant, Variable, Functor and StructuredTerm
  *
  *
  * */


trait Named {
  val name: String

  require(name != "" && !name.contains(" "), s"Invalid name for term: $name")

  def getName: String = name
}

trait WithArguments {
  protected val arguments: Seq[Term]

  require(arguments.nonEmpty, s"Cannot pass an empty list as argument")

  def getArguments: Seq[Term] = arguments

  def getArgumentSort: Seq[Sort] = arguments.map(_.getSort)

  def isGround: Boolean = !arguments.exists {
    case x: Variable => true
    case x: Structure => x.vars.nonEmpty
    case _ => false
  }

  def vars: Seq[Variable] = arguments.collect({
    case x: Variable => Seq(x)
    case x: Structure => x.vars
  }).flatten

}

trait WithSort {
  val sort: Sort

  def getSort: Sort = sort
}

trait Prob {
  protected val probability: Double
  require(probability >= 0, s"Probability cannot be < 0 ($probability)")
  require(probability <= 1, s"Probability cannot be > 1 ($probability)")

  def getProbability: Double = probability
}

trait Weight {
  protected val weight: Double

  def getWeight: Double = weight
}

abstract class Term(override protected val name: String,
                    override protected val sort: Sort) extends Named with WithSort {

}

abstract class SimpleTerm(override protected val name: String,
                          override protected val sort: Sort) extends Term(name, sort) {

}

abstract class ComplexTerm(override protected val name: String,
                           override protected val sort: Sort,
                           override protected val arguments: Term*) extends Term(name, sort) with WithArguments {

}
