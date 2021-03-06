package ScalaLogic.language.commons

import java.io.PrintWriter

import ScalaLogic.language.lp.Clause

class Interpretation {

  val facts: collection.mutable.Map[Predicate, collection.mutable.Set[Atom]] = collection.mutable.Map()
  val rules: collection.mutable.Set[Clause] = collection.mutable.Set()

  def addFact(fact: Atom): Unit = {
    require(fact.isGround, s"Atom has to be grounded ($fact)")

    if (!facts.contains(fact.getPredicate)) {
      facts(fact.getPredicate) = collection.mutable.Set()
    }
    facts(fact.getPredicate) add fact
  }

  def addRule(rule: Clause): Unit = {
    rules add rule
  }

  def getFacts: Seq[Atom] = {
    facts.foldLeft(Seq[Atom]())((acc, t) => acc ++ t._2)
  }

  def getRules: Seq[Clause] = {
    rules.toSeq
  }

  def prepareFile(path: String): Unit = {
    val printer = new PrintWriter(path)

    getFacts.foreach(f => printer.write(f.toString + ".\n"))
    getRules.foreach(r => printer.write(r.toString + ".\n"))

    printer.close()
  }

}
