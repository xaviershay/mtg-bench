package mtg

trait Targetable
class Player(val agent : Agent) extends Targetable {
  val hand      = new Hand
  val library   = new Library
  val graveyard = new Graveyard
  val sideboard = new Sideboard
  var landsPlayedThisTurn = 0
  var life = 3

  def takeDamage(state : GameState, amount : Int) {
    // TODO: Triggers
    life -= amount
  }
}
