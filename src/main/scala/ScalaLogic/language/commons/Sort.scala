package ScalaLogic.language.commons

class Sort(name: String) {

  val elements: collection.mutable.Set[Constant] = collection.mutable.Set()

  def addElement(element: Constant): Sort.this.elements.type = {
    elements += element
  }

  def contains(element: Constant): Boolean = {
    elements contains element
  }

  def getName: String = name

  def getElements: collection.mutable.Set[Constant] = { elements }

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
