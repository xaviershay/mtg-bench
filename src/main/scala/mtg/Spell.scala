package mtg

abstract class Spell(
  val source : Card,
  val targets : List[Targetable],
  val payments : List[Action.Payment]
) {
  def execute(state : GameState) : List[Effect]
}
