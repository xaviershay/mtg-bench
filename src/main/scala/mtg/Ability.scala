package mtg

trait ManaAbility {
  val card : Card
}
class Ability(val card : Card)
class TapForOneMana(override val card: Card, val color : Color) extends Ability(card) with ManaAbility {
  override def toString = { "Tap for one " + color }
}

