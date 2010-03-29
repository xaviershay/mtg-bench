import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers

import mtg._

class TurnSpec extends Spec with MustMatchers {
  class MockSpell(
    source : Card,
    targets : List[Targetable],
    payments : List[Action.Payment]
  ) extends Spell(source, targets, payments) {
    def execute(state : GameState) : List[Effect] = { List() }
  }

  describe("Resolving an instant") {
    it("moves card to it's owner's graveyard") {
      val game = new GameMaster
      val state = game.state
      val castable = new LightningBolt
      state.players = List(new Player(new DumbAgent("test")))
      castable.controller = Some(state.players.head)
      state.stack.add(castable)
      state.spellStack = List(new MockSpell(castable, List(), List()))

      game.resolveTopSpellOnStack

      state.stack.cards.isEmpty must equal(true)
      state.spellStack.isEmpty  must equal(true)
      castable.controller match {
        case Some(player) => player.graveyard.cards must equal(List(castable))
        case _ => fail("fail")
      }
    }
  }
}

