package ScalaLogic.language.commons


sealed abstract class BinToken {
  def isCommutative: Boolean =
    List(OR, AND) contains this

  def isAssociative: Boolean =
    List(OR, AND) contains this
}


case object OR extends BinToken
case object AND extends BinToken
case object IMPL extends BinToken
case object EQV extends BinToken
