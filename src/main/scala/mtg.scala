import scala._

class Card {
  val location : Option[Location] = None
  val name = "A card"
  override def toString = { "Hey I'm a card" }
}

trait Color { }
trait Red extends Color {}
trait Green extends Color {}
trait Blue extends Color {}
trait White extends Color {}
trait Black extends Color {}

trait Creature {}

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
    name + "\n" + cards.map{"  " + _.name}.foldLeft("") { _ + "\n" + _ }
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

object Mtg {
  def main(args: Array[String]) {
    val state = new GameState
    state.hand.add( new Card with Red with Creature)
    //val other = new Card with Colored[Green] with Creature
    //val yourCard = new Card with Colored[Red] with Creature


    //myHand.add(yourCard)
    println(state.hand)
  }
}
