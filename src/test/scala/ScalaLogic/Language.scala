package ScalaLogic

import ScalaLogic.language.commons.{Atom, Constant, Predicate, Sort, Variable}
import org.junit.runner.RunWith
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Language extends FunSpec with Matchers {

  describe("Logic commons:") {

    it ("Primitives are OK") {
      val person = Sort("Person")
      val alice = Constant("alice", person)
      val bob = Constant("bob", person)
      val friends = Predicate("friends", Seq(person, person): _*)
      val variable = Variable("X", person)

      val atom = friends(Seq(alice, bob): _*)
      val atom2 = friends(Seq(bob, variable): _*)

      friends.arity should be (2)
      person.size should be (2)
      atom.isInstanceOf[Atom] should be (true)
      atom.toString should be ("friends(alice,bob)")
      atom2.toString should be ("friends(bob,X)")
    }

    it ("Constant has to be lower case") {
      an [java.lang.AssertionError] should be thrownBy Constant("Alice")
    }

    it ("Variable should be uppercase") {
      an [java.lang.AssertionError] should be thrownBy Variable("x")
    }
  }





  describe("Logic programs") {
    it ("Constructing clauses") {
      val varX = Variable("X")
      val varY = Variable("Y")
      val alice = Constant("alice")
      val friends = Predicate("friends", 2)
      val smokes = Predicate("smokes", 1)

      val head = smokes(Seq(varX): _*)
      val friendsAtom = friends(Seq(varX, varY): _*)
      val smokesAtom = smokes(Seq(varY): _*)


      val clause1 = head :- friendsAtom
      val clause2 = head :- (Seq(friendsAtom, smokesAtom): _*)
      val clause3 = head :- friendsAtom :^ smokesAtom

      clause2.toString should be ("smokes(X) :- friends(X,Y),smokes(Y)")
      clause1.toString should be ("smokes(X) :- friends(X,Y)")
      clause3.toString should be ("smokes(X) :- friends(X,Y),smokes(Y)")

    }
  }


}
