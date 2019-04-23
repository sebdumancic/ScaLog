package ScalaLogic.language.commons

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
