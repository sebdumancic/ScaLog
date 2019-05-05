package ScalaLogic.language.fol

sealed abstract class QuToken {

  def isUniversal: Boolean = { this == FORALL }

  def isExistential: Boolean = { this == EXISTS }

}


case object FORALL extends QuToken
case object EXISTS extends QuToken

