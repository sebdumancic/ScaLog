package ScalaLogic.language.commons

class Sort(name: String) {

  val elements: collection.mutable.Set[Term] = collection.mutable.Set()

  def addElement(element: Term): Sort.this.elements.type = {
    elements += element
  }

  def contains(element: Term): Boolean = {
    elements contains element
  }

  def getName: String = name

  def getElements: Set[Term] = { elements.toSet }

  def size: Int = elements.size

  override def toString: String = s"$name($size): ${elements.mkString(",")}"

  override def equals(obj: Any): Boolean = obj match {
    case that: Sort => size.compare(that.size) == 0 & that.getElements.forall(elements.contains)
    case _ => false
  }

  override def hashCode(): Int = (name, getElements.##).##

}

object Sort {
  val sortCache: collection.mutable.Map[String, Sort] = collection.mutable.Map()

  def apply(name: String): Sort = {
    if (!sortCache.contains(name)) {
      sortCache(name) = new Sort(name)
    }
    sortCache(name)
  }
}
