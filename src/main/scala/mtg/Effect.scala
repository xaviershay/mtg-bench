package mtg

abstract class Effect {
  def execute(state : GameState)
}

object Effect {
  class DamageToPlayer(val player : Player, val amount : Int) extends Effect {
    override def execute(state : GameState) {
      player.takeDamage(state, amount)
    }
  }
}

abstract class TargetedEffect[TargetType] {
}
class Target
object Target {
  class Player extends Target
}
