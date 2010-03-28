import scala._
import mtg._
import mtg.color._
import mtg.card_type._

class GameMaster {
  var agents : List[Agent] = List()
  val state = new GameState

  def registerAgent(agent : Agent) {
    agents = agent :: agents
  }

  def start() {
    println("Starting game")
    agents.foreach(agent => {
      var player = new Player(agent)
      player.agent.deck
      state.players = player :: state.players
      agent.deck.foreach(card => {
        player.library.add(card)
      })
      player.library.shuffle
      println("Registered library for " + agent)
      println(player.library)
    })

    println("\nDealing")
    state.players.foreach(player => {
      player.hand.add(player.library.draw)
      player.hand.add(player.library.draw)
      player.hand.add(player.library.draw)
      player.agent.receiveHand(player.hand)
    })

    // TODO: This doesn't work for a continuous game loop
    state.players.foreach(currentTurn => {
      // TODO: Untap step
      // Draw step
      var actions = currentTurn.agent.receivePriority(
        new PublicGameState(Step.Draw, currentTurn, state.battlefield.cards),
        new PrivateGameState(currentTurn))
      actions.foreach(_.execute(state, currentTurn))

      // Main step
      var allPassed = false
      while (allPassed == false) {
        allPassed = true
        state.players.foreach(player => {
          var actions = player.agent.receivePriority(
            new PublicGameState(Step.PrecombatMain, currentTurn, state.battlefield.cards),
            new PrivateGameState(player))
          actions.foreach(_.execute(state, player))
          if (!actions.isEmpty)
            allPassed = false
        })
      }

      // End turn
      allPassed = false
      var stackEmpty = false
      println("END TURN")
      while (!stackEmpty) {
        while (!allPassed) {
          println("Starting round of priority")
          allPassed = true
          state.players.foreach(player => {
            // Check SB effects
            if (state.players.exists{ (a) => println("Life: " + a.life); a.life <= 0}) {
              println("THE GAME IS OVER")
              return
            }

            var actions = player.agent.receivePriority(
              new PublicGameState(Step.End, currentTurn, state.battlefield.cards),
              new PrivateGameState(player))
            actions.foreach(_.execute(state, player))
            if (!actions.isEmpty)
              allPassed = false
          })
        }

        stackEmpty = state.spellStack.isEmpty
        if (!stackEmpty) {
          allPassed = false
          var toResolve = state.spellStack.head

          var effects = toResolve.execute(state)
          effects.foreach(_.execute(state))
          toResolve.source.controller match { // TODO: WRONG
            case Some(player) => player.graveyard.add(toResolve.source)
            case None => {}
          }
          state.spellStack = state.spellStack.tail
        }
      }
    })
  }
}





object Mtg {
  def main(args: Array[String]) {
    val game = new GameMaster
    game.registerAgent(new DumbAgent("fred"))
    game.start
   /*
    val state = new GameState
    val card = new LightningBolt
    val forest = new Forest
    state.inPlay.add( card )
    state.inPlay.add( forest )
    state.inPlay.add( new SpreadingSeas(forest) )
    //val other = new Card with Colored[Green] with Creature
    //val yourCard = new Card with Colored[Red] with Creature
    //myHand.add(yourCard)
    println(state.inPlay)
    */
  }
}
