package ScalaLogic.language.commons

/**
  * Implements the basic Term functionality, base class for Constant, Variable, Functor and StructuredTerm
  *
  *
  * */



trait Named {
  protected val name: String

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
  protected val sort: Sort

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






abstract class Term(protected val name: String,
                    protected val sort: Sort) extends Named with WithSort {

}

abstract class SimpleTerm(override protected val name: String,
                          override protected val sort: Sort) extends Term(name, sort) {

}

abstract class ComplexTerm(override protected val name: String,
                           override protected val sort: Sort,
                           override protected val arguments: Term*) extends Term(name, sort) with WithArguments {

}


/**
  *
  *         CONSTANT
  *
  *
  */



case class Constant(override protected val name: String,
                    override protected val sort: Sort) extends SimpleTerm(name, sort) {

  assert(name.head.isLower, s"Constants should be lowercase ($name)")
  sort.addElement(this)

  override def toString: String = name

  override def equals(obj: Any): Boolean = obj match {
    case that: Constant => name == that.getName && sort == that.getSort
    case _ => false
  }

  override def hashCode(): Int = (name, sort.getName).##

}

object Constant {
  def apply(name: String, sort: Sort) =
    new Constant(name, sort)

  def apply(name: String): Constant = apply(name, Sort("Thing"))
}





/**
  *
  *         VARIABLE
  *
  * */




case class Variable(override protected val name: String,
                    override protected val sort: Sort) extends SimpleTerm(name, sort) {

  assert(name.head.isUpper, s"Variables should be uppercase ($name)")

  override def toString: String = name

  override def equals(obj: Any): Boolean = obj match {
    case that: Variable => getName == that.getName & getSort == that.getSort
    case _ => false
  }

  override def hashCode(): Int = (getName, getSort).##

}

object Variable {
  val variableCache: collection.mutable.Map[(String, Sort), Variable] = collection.mutable.Map()

  def apply(name: String, sort: Sort): Variable = {
    if (!variableCache.contains((name, sort))) {
      variableCache((name, sort)) = new Variable(name, sort)
    }
    variableCache((name, sort))
  }

  def apply(name: String): Variable = {
    apply(name, Sort("Thing"))
  }
}






/**
  *
  *         STRUCTURE / COMPOUND TERM
  *
  * */


case class Structure(override protected val name: String,
                     override protected val sort: Sort,
                     override protected val arguments: Term*) extends ComplexTerm(name, sort, arguments: _*) {

  override def toString: String = s"$name(${arguments.toString().mkString(",")})"

  override def equals(obj: Any): Boolean = obj match {
    case that: Structure => that.getName == name && !that.getArguments.zip(arguments).exists(t => t._1 != t._2)
    case _ => false
  }
}


object Structure {

  def apply(name: String, sort: Sort, arguments: Term*): Structure = {
    new Structure(name, sort, arguments: _*)
  }

  def apply(name: String, arguments: Term*): Structure = {
    new Structure(name, Sort(name), arguments: _*)
  }
}