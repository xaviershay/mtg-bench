import scala._
import mtg._

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
      player.hand.add(player.library.draw)
      player.hand.add(player.library.draw)
      player.hand.add(player.library.draw)
      player.hand.add(player.library.draw)
      player.agent.receiveHand(player.hand)
    })

    try {
      while(true) {
        state.players.foreach(player => runTurn(player))
      }
    } catch {
      case e : mtg.GameOver => println("Game over")
    }
  }

  def runTurn(currentTurn : Player) {
    println("\nNEW TURN - " + currentTurn.life + " life")

    // TODO: Untap step
    state.battlefield.cards.filter(
      _.controller.getOrElse(null) == currentTurn
    ).foreach(_.tapped = false)
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
    while (!stackEmpty) {
      while (!allPassed) {
        allPassed = true
        state.players.foreach(player => {
          // Check SB effects
          if (state.players.exists{ (a) => a.life <= 0}) {
            throw new GameOver
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
        resolveTopSpellOnStack
      }
    }

    // Cleanup
    currentTurn.landsPlayedThisTurn = 0
  }

  def resolveTopSpellOnStack {
    val toResolve = state.spellStack.head
    val effects = toResolve.execute(state)

    effects.foreach(_.execute(state))
    toResolve.source.controller match { // TODO: WRONG
      case Some(player) => player.graveyard.add(toResolve.source)
      case None => {}
    }
    state.spellStack = state.spellStack.tail
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
