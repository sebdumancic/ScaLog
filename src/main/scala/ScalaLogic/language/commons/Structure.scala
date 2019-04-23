package ScalaLogic.language.commons

class Structure(override val name: String,
                override val arguments: SimpleTerm*) extends ComplexTerm(name, arguments: _*) {

  override def toString: String = s"$name(${arguments.toString().mkString(",")})"

  override def equals(obj: Any): Boolean = obj match {
    case that: Structure => that.getName == name && !that.getArguments.zip(arguments).exists(t => t._1 != t._2)
    case _ => false
  }
}
