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
  protected val arguments: Seq[SimpleTerm]

  require(arguments.nonEmpty, s"Cannot pass an empty list as argument")

  def getArguments: Seq[SimpleTerm] = arguments

  def getArgumentSort: Seq[Sort] = arguments.map(_.getSort)

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

abstract class Term(val name: String) extends Named {

}

abstract class SimpleTerm(override val name: String,
                          override val sort: Sort) extends Term(name) with WithSort {

}

abstract class ComplexTerm(name: String, arguments: Term*) extends Term(name) with WithArguments {

}
