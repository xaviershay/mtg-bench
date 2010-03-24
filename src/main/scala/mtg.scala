import scala._

class Color { }
class Red extends Color {}
object Red extends Red { override def toString = { "Red" } }
class Green extends Color {}
object Green extends Green { override def toString = { "Green" } }
class Blue extends Color {}
object Blue extends Blue{ override def toString = { "Blue" } }
class White extends Color {}
class Black extends Color {}

class CardType
class Instant extends CardType
object Instant extends Instant
class Land extends CardType
object Land extends Land
class Enchantment extends CardType
object Enchantment extends Enchantment


class Ability
class TapForOneMana(color : Color) extends Ability {
  override def toString = { "Tap for one " + color }
}

abstract class Card {
  def color : List[Color] = {
    intercepts.foldLeft(baseColors) {
      case (a : List[_], b : ColorInterceptor) => b.interceptColor(a)
      case (a : List[_], _) => a
    }
  }
  def cardType : List[CardType] = {
    baseTypes
  }
  def abilities : List[Ability] = {
    intercepts.foldLeft(baseAbilities) {
      case (a : List[_], b : AbilityInterceptor) => b.interceptAbilities(a)
      case (a : List[_], _) => a
    }
  }

  val baseColors    : List[Color]    = List()
  val baseAbilities : List[Ability]  = List()
  val baseTypes     : List[CardType]


  var intercepts : List[Interceptor] = List()
  def registerIntercept(intercept : Interceptor) {
    intercepts = intercept :: intercepts
  }

  val location : Option[Location] = None
  val name = "unknown card"
  override def toString = {
    name + " (" + color + ")" + abilities.map { "   - " + _ }.foldLeft("") { _ + "\n" + _ }
  }
}

trait Interceptor
trait ColorInterceptor extends Interceptor {
  def interceptColor(existingColors : List[Color]) : List[Color]
}

trait AbilityInterceptor extends Interceptor {
  def interceptAbilities(existingAbilities : List[Ability]) : List[Ability]
}

class LightningBolt extends Card {
  override val baseColors = List(Red)
  override val baseTypes  = List(Instant)
  override val name = "Lightning Bolt"
}

class Forest extends Card {
  override val baseColors = List(Green)
  override val baseTypes  = List(Land)
  override val baseAbilities : List[Ability] = List(new TapForOneMana(Green))
  override val name = "Forest"
}

class SpreadingSeas(target : Card) extends Card with ColorInterceptor with AbilityInterceptor {
  target.registerIntercept(this)

  override val baseColors = List(Blue)
  override val baseTypes  = List(Enchantment)
  override val name = "Spreading Seas"

  override def interceptColor(existingColors : List[Color]) : List[Color] = {
    List(Blue)
  }

  override def interceptAbilities(existingAbilities : List[Ability]) : List[Ability] = {
    List(new TapForOneMana(Blue))
  }
}

class Player {}
class PlayerA extends Player {}
class PlayerB extends Player {}

abstract class Location {
  val name : String
  var cards : List[Card] = List()

  def add(card : Card) {
    card.location match {
      case Some(l) => l.remove(card)
      case None    => {}
    }
    cards = card :: cards
  }

  def remove(card : Card) {
    cards = cards - card
  }

  override def toString = {
    name + "\n" + cards.map{"  " + _}.foldLeft("") { _ + "\n" + _ }
  }
}
class Hand extends Location {
  val name = "Hand"
}
class InPlay extends Location {
  val name = "InPlay"
}

class GameState {
  var cards : List[Card]  = List()
  val hand   = new Hand
  val inPlay = new InPlay
}

abstract class TargetedEffect[TargetType] {
}


object Mtg {
  def main(args: Array[String]) {
    val state = new GameState
    val card = new LightningBolt
    val forest = new Forest
    state.hand.add( card )
    state.hand.add( forest )
    state.hand.add( new SpreadingSeas(forest) )
    //val other = new Card with Colored[Green] with Creature
    //val yourCard = new Card with Colored[Red] with Creature
    //myHand.add(yourCard)
    println(state.hand)
  }
}
