package mtg


class ListExtensions[A <: AnyRef](xs : List[A]) {
  def randomPermute: List[A] = {
    val rand = new util.Random
    val a = xs.toArray
    for (i <- a.length - 1 to 1 by -1) {
      val i1 = rand.nextInt(i + 1)
      val t = a(i)
      a.update(i, a(i1))
      a.update(i1, t)
    }
    a.toList
  }
}

object Implicits {
  implicit def listExtensions[A <: AnyRef](xs : List[A]) = new ListExtensions(xs)
}
import Implicits._

abstract class Location {
  val name : String
  var cards : List[Card] = List()

  def add(card : Card) {
    card.location match {
      case Some(l) => l.remove(card)
      case None    => {}
    }
    card.location = Some(this)
    cards = card :: cards
  }

  def remove(card : Card) {
    card.location = None
    cards = cards - card
  }

  override def toString = {
    (name :: cards.map{"  " + _}).mkString("\n")
  }
}
class Unknown extends Location { val name = "Unknown" }
class Hand extends Location { val name = "Hand" }
class Battlefield extends Location { val name = "Battlefield" }
class Exile extends Location { val name = "Exile" }
class Graveyard extends Location { val name = "Graveyard" }
class Sideboard extends Location { val name = "Sideboard" }
class Stack extends Location {
  val name = "Stack"

  def push(card : Card) {
    add(card)
  }

  def top : Card = {
    cards.reverse.head
  }
}
class Library extends Location {
  val name = "Library"

  def shuffle {
    cards = cards.randomPermute
  }

  def draw : Card = {
    val ret = cards.head
    cards = cards.tail
    ret
  }
}

