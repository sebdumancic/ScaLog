package ScalaLogic

import ScalaLogic.language.commons.{Constant, Interpretation, Predicate, Sort, Variable}
import ScalaLogic.solvers.lp.SWIPL
import org.junit.runner.RunWith
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Solvers extends FunSpec with Matchers {

  describe ("Basic SWIPL test") {
    val person = Sort("Person")
    val alice = Constant("alice", person)
    val bob = Constant("bob", person)
    val friends = Predicate("friends", Seq(person, person): _*)
    val smokes = Predicate("smokes", person)

    val varX = Variable("X", person)
    val varY = Variable("Y", person)

    val friends_XY = friends(Seq(varX, varY): _*)
    val smokes_Y = smokes(varY)
    val smokes_X = smokes(varX)

    val clause = smokes_X :- friends_XY :^ smokes_Y

    val friends_bobalice = friends(Seq(alice, bob): _*)
    val smokes_bob = smokes(bob)

    val kb = new Interpretation()
    kb.addFact(friends_bobalice)
    kb.addFact(smokes_bob)
    kb.addRule(clause)

    val prolog = new SWIPL(kb)

    it ("SWIPL should answer correctly") {

      prolog.hasSolution(smokes_X) should be (true)

      prolog.oneSolution(smokes_X) should be (Map(varX -> bob))

      prolog.allSolutions(smokes_X) should contain allOf (Map(varX -> bob), Map(varX -> alice))

    }



  }

}
