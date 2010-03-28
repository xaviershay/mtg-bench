package mtg

class Step
object Step {
  object Untap extends Step
  object Draw extends Step
  object Upkeep extends Step
  object PrecombatMain extends Step
  object Combat extends Step
  object PostcombatMain extends Step
  object End extends Step
  object Cleanup extends Step
}
