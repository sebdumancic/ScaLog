package ScalaLogic.language.commons

class Structure(override protected val name: String,
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
