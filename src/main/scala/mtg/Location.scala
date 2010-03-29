package mtg


object ListExtensions {
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
  }

  def randomSelect[A](n: Int, ls: List[A]): List[A] =
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
      e :: randomSelect(n - 1, rest)
    }

  def randomPermute[A](ls : List[A]) : List[A] = {
    randomSelect(ls.length, ls)
  }
}

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
    cards = ListExtensions.randomPermute(cards)
  }

  def draw : Card = {
    if (cards.isEmpty) throw new GameOver
    val ret = cards.head
    cards = cards.tail
    ret
  }
}

