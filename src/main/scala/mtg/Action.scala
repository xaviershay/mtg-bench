package mtg
import mtg._

abstract class Action {
  def execute(state : GameState, owner : Player)
}

object Action {
  class DrawCard extends Action {
    override def execute(state : GameState, owner : Player) {
      val card = owner.library.draw
      owner.hand.add(card)
      println(owner.agent + " drew a card")
    }
  }

  class PlayLand(land : Card) extends Action {
    override def execute(state : GameState, owner : Player) {
      if (owner.landsPlayedThisTurn < 1) {
        state.battlefield.add(land)
        land.controller = Some(owner)
        owner.landsPlayedThisTurn += 1
        println(owner.agent + " played a land: " + land)
      }
    }
  }

  class ActivateManaAbility(ability : Ability) extends Action {
    override def execute(state : GameState, owner : Player) {
      // TODO: Do this right - check the ability activation cost
      if (!ability.card.tapped)
        ability.card.tapped = true
    }
  }

  class Cast(
    card : Castable,
    determineTarget: (Target) => Player,
    determineCost: () => List[Cost],
    activateManaAbilities: () => List[ActivateManaAbility],
    payments: () => List[Payment]
  ) extends Action {
    override def execute(state : GameState, owner : Player) {
      card.controller = Some(owner) // 601.2a
      state.stack.add(card) // 601.2a
      println(owner.agent + " casting a spell: " + card)
      // 601.2c choose targets
      // Just does players for now
      var targetPlayer = determineTarget(new Target.Player)

      // 601.2e determine cost
      //   Choose X, sacrifice
      //   For now, agent reads off card
      var cost = determineCost()
      // 601.2f activate mana abilities
      //   Put mana in pool
      var abilities = activateManaAbilities()
      abilities.foreach(_.execute(state, owner)) // TODO: Triggers?

      // 601.2g pay cost
      //   List[Payment] -> ManaPayment[Red](2)
      var paymentsMade = payments()
      paymentsMade.foreach(_.execute(state, owner))

      state.spellStack = card.makeSpell(List(targetPlayer), paymentsMade) :: state.spellStack

      // 601.2h trigger abilities for when a spell is cast

    }
  }

  abstract class Payment extends Action
  class RemoveManaFromPool[T <: Color](amount : Int) extends Payment {
    override def execute(state : GameState, owner : Player) {
    }
  }
}
