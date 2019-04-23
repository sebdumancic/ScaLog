package ScalaLogic.language.commons

class Predicate(name: String, argumentSorts: Sort*) {

  def getName: String = name

  def arity: Int = argumentSorts.length

  def getSorts: Seq[Sort] = argumentSorts

  def getSignature: String = s"$name/$arity"

  def apply(args: SimpleTerm*): Atom = {
    assert(args.lengthCompare(argumentSorts.length) == 0, s"Expecting $arity argument, received ${args.length}")
    assert(args.map(_.getSort) == argumentSorts,
      s"Expected the following types: ${argumentSorts.mkString(",")}, received: ${args.mkString(",")}")
    Atom(this, args: _*)
  }

  override def toString: String = s"$name(${getSorts.map(_.getName).mkString(",")})"

  override def equals(obj: Any): Boolean = obj match {
    case that: Predicate => getName == that.getName & getSorts == that.getSorts
    case _ => false
  }

  override def hashCode(): Int = (getName, getSorts.##).##


}

object Predicate {
  val cache: collection.mutable.Map[(String, Seq[Sort]), Predicate] = collection.mutable.Map()

  def apply(symbol: String, argumentSorts: Sort*): Predicate = {
    if (!cache.contains(symbol, argumentSorts)) {
      cache((symbol, argumentSorts)) = new Predicate(symbol, argumentSorts: _*)
    }
    cache((symbol, argumentSorts))
  }

  def apply(symbol: String, arity: Int): Predicate = {
    val args = for (_ <- 0 until arity) yield Sort("Thing")
    apply(symbol, args: _*)
  }
}
