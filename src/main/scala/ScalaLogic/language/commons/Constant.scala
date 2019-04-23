package ScalaLogic.language.commons

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
