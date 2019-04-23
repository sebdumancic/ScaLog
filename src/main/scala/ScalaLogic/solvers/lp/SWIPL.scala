package ScalaLogic.solvers.lp

import ScalaLogic.language.commons
import ScalaLogic.language.commons.{Constant, KnowledgeBase, Structure}


class SWIPL(override val knowledgeBase: KnowledgeBase) extends Prolog(knowledgeBase) {

  import org.jpl7._
  prepareKnowledge()

  protected def prepareKnowledge(): Unit = {

    knowledgeBase.getFacts.foreach(fact => {
      Query.hasSolution(s"assertz(${fact.toString})")
    })

    knowledgeBase.getRules.foreach(rule => {
      Query.hasSolution(s"assertz((${rule.toString}))")
    })

  }

  protected def argToJPL(arg: commons.Term): Term = arg match {
    case t: Constant => new Atom(t.getName)
    case t: commons.Variable => new Variable(t.getName)
    case t: Structure => new Compound(t.getName, t.getArguments.map(argToJPL).toArray)
  }

  protected def queryToJPL(atom: commons.Atom): Query = atom match {
    case atm: commons.Atom =>
      new Query(new Compound(atm.getPredicate.getName,
                atm.getArguments.map(argToJPL).toArray))
    case _ => throw new NotImplementedError(s"Querying formulas not supported yet")
  }

  override def hasSolution(queryAtom: commons.Atom): Boolean = {
    val hs = queryToJPL(queryAtom)
    try {
      hs.hasSolution
    }
    finally {
      hs.close()
    }
  }

  override def oneSolution(queryAtom: commons.Atom): Map[commons.Variable, Constant] = {
    val os = queryToJPL(queryAtom)
    try {
      val res = os.oneSolution()
      queryAtom.vars.map(v => (v, Constant(res.get(v.getName).name(), v.getSort))).toMap
    }
    finally {
      os.close()
    }
  }

  override def allSolutions(queryAtom: commons.Atom): Seq[Map[commons.Variable, Constant]] = {
    val as = queryToJPL(queryAtom)
    try {
      val res = as.allSolutions()
      res.map(rr => {
        queryAtom.vars.map(v => (v, Constant(rr.get(v.getName).name(), v.getSort))).toMap
      })
    }
    finally {
      as.close()
    }
  }

}
