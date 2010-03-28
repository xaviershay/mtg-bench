package mtg

class GameState {
  var cards : List[Card]  = List()
  var players : List[Player] = List()
  val battlefield = new Battlefield
  val exile = new Exile
  val stack = new Stack
  var spellStack : List[Spell] = List()
  var currentStep = Step.Untap
}

class PublicGameState(
  val currentStep : Step,
  val currentTurn : Player,
  val battlefield : List[Card])

class PrivateGameState(val player : Player) {
  def hand = { player.hand }
}
