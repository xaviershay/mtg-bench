import scala._

class ListExtensions[A <: AnyRef](xs : List[A]) {
  def join(separator : String) : String = {
    if (xs.isEmpty)
      ""
    else
      xs.tail.foldLeft(xs.head.toString) { (acc : String, a : A) => acc + separator + a.toString }
      //List[A] -> B -> ((B, A) -> B)
  }

  def randomPermute: List[A] = {
    val rand = new util.Random
    val a = xs.toArray
    for (i <- a.length - 1 to 1 by -1) {
      val i1 = rand.nextInt(i + 1)
      val t = a(i)
      a.update(i, a(i1))
      a.update(i1, t)
    }
    a.toList
  }
}

object Implicits {
  implicit def listExtensions[A <: AnyRef](xs : List[A]) = new ListExtensions(xs)
}
import Implicits._

class Color { }
class Red extends Color {}
object Red extends Red { override def toString = { "Red" } }
class Green extends Color {}
object Green extends Green { override def toString = { "Green" } }
class Blue extends Color {}
object Blue extends Blue{ override def toString = { "Blue" } }
class White extends Color {}
class Black extends Color {}

class CardType
class Instant extends CardType
object Instant extends Instant
class Land extends CardType
object Land extends Land
class Enchantment extends CardType
object Enchantment extends Enchantment


trait ManaAbility {
  val card : Card
}
class Ability(val card : Card)
class TapForOneMana(override val card: Card, val color : Color) extends Ability(card) with ManaAbility {
  override def toString = { "Tap for one " + color }
}

abstract class Card {
  def color : List[Color] = {
    intercepts.foldLeft(baseColors) {
      case (a : List[_], b : ColorInterceptor) => b.interceptColor(a)
      case (a : List[_], _) => a
    }
  }
  def cardTypes(state : PublicGameState) : List[CardType] = {
    baseTypes
  }
  def abilities : List[Ability] = {
    intercepts.foldLeft(baseAbilities) {
      case (a : List[_], b : AbilityInterceptor) => b.interceptAbilities(a)
      case (a : List[_], _) => a
    }
  }

  val baseColors    : List[Color]    = List()
  val baseAbilities : List[Ability]  = List()
  val baseTypes     : List[CardType]


  var intercepts : List[Interceptor] = List()
  def registerIntercept(intercept : Interceptor) {
    intercepts = intercept :: intercepts
  }

  var location : Option[Location] = None
  var controller : Option[Player] = None
  var owner = { controller } // TODO: WRONG
  var tapped = false

  val name = "unknown card"
  override def toString = {
    name + " (" + color.mkString(", ") + ") (" + location.getOrElse(new Unknown).name + ")"
  }

  def resolve(state : GameState) : List[Effect] = {
    List()
  }
}


trait Interceptor
trait ColorInterceptor extends Interceptor {
  def interceptColor(existingColors : List[Color]) : List[Color]
}

trait AbilityInterceptor extends Interceptor {
  def interceptAbilities(existingAbilities : List[Ability]) : List[Ability]
}

class LightningBolt extends Card {
  override val baseColors = List(Red)
  override val baseTypes  = List(Instant)
  override def resolve(state : GameState) = {
    controller match {
      case Some(player) => List(new Effect.DamageToPlayer(player, 3))
      case None => List()
    }
  }
  override val name = "Lightning Bolt"
}

class Forest extends Card {
  override val baseColors = List(Green)
  override val baseTypes  = List(Land)
  override val baseAbilities : List[Ability] = List(new TapForOneMana(this, Green))
  override val name = "Forest"
}

class Mountain extends Card {
  override val baseColors = List(Red)
  override val baseTypes  = List(Land)
  override val baseAbilities : List[Ability] = List(new TapForOneMana(this, Red))
  override val name = "Mountain"
}

class SpreadingSeas(target : Card) extends Card with ColorInterceptor with AbilityInterceptor {
  target.registerIntercept(this)

  override val baseColors = List(Blue)
  override val baseTypes  = List(Enchantment)
  override val name = "Spreading Seas"

  override def interceptColor(existingColors : List[Color]) : List[Color] = {
    List(Blue)
  }

  override def interceptAbilities(existingAbilities : List[Ability]) : List[Ability] = {
    List(new TapForOneMana(target, Blue))
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
    cards = cards.randomPermute
  }

  def draw : Card = {
    val ret = cards.head
    cards = cards.tail
    ret
  }
}

class GameState {
  var cards : List[Card]  = List()
  var players : List[Player] = List()
  val battlefield = new Battlefield
  val exile = new Exile
  val stack = new Stack
  var currentStep = Step.Untap
}

class PublicGameState(
  val currentStep : Step,
  val currentTurn : Player,
  val battlefield : List[Card])

class PrivateGameState(val player : Player) {
  def hand = { player.hand }
}

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
        println("Stack: " + state.stack)
        stackEmpty = state.stack.cards.isEmpty
        if (!stackEmpty) {
          allPassed = false
          var toResolve = state.stack.top

          var effects = toResolve.resolve(state)
          effects.foreach(_.execute(state))
          toResolve.controller match { // TODO: WRONG
            case Some(player) => player.graveyard.add(toResolve)
            case None => {}
          }
        }
      }
    })
  }
}


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
    card : Card,
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
      payments().foreach(_.execute(state, owner))

      // 601.2h trigger abilities for when a spell is cast

    }
  }

  abstract class Payment extends Action
  class RemoveManaFromPool[T <: Color](amount : Int) extends Payment {
    override def execute(state : GameState, owner : Player) {
    }
  }
}

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

class Target
object Target {
  class Player extends Target
}

class Cost
class ManaPayment[T <: Color](amount : Int) extends Cost


abstract class Agent {
  /* Called in setup to get the card list of this agent */
  def deck : List[Card]

  /* Called at start of game. TODO: Mulligans */
  def receiveHand(hand : Hand)

  /* Main game loop, called every time this agent receives priority */
  def receivePriority(state : PublicGameState, me : PrivateGameState) : List[Action]
}
class DumbAgent(name :String) extends Agent {
  override def deck : List[Card] = {
    List(
      new Mountain,
      new Mountain,
      new LightningBolt,
      new LightningBolt,
      new LightningBolt,
      new LightningBolt
    )
  }

  override def receiveHand(hand : Hand) {
    println("Received Hand:")
    println(hand)
  }
  override def receivePriority(state : PublicGameState, me : PrivateGameState) : List[Action] = {
    state.currentStep match {
      case Step.Draw => {
        List(new Action.DrawCard)
      }
      case Step.PrecombatMain => {
        if (state.currentTurn != me.player || me.player.landsPlayedThisTurn >= 1) {
          List()
        } else {
          var land = me.hand.cards.find(_.cardTypes(state).exists(_ match {
            case Land => true
            case _    => false
          }))
          land match {
            case Some(land) => List(new Action.PlayLand(land))
            case None    => List()
          }
        }
      }
      case Step.End => {
        var mountain = state.battlefield.filter(
          _.controller.getOrElse(null) == me.player
        ).find(_ match {
          case c : Mountain => !c.tapped
          case _ => false
        })
        var spell = me.hand.cards.find(_ match {
          case c : LightningBolt => { true}
          case a    => { false }
        })
        mountain match {
          case Some(mountain) => {
            spell match {
              case Some(spell) => {
                List(new Action.Cast(
                  spell,
                  (target : Target) => {
                    target match {
                      case p : Target.Player => me.player
                    }
                  },
                  () => List(new ManaPayment[Red](1)),
                  () => List(new Action.ActivateManaAbility(mountain.abilities.head)),
                  () => List(new Action.RemoveManaFromPool[Red](1))))
              }
              case None => List()
            }
          }
          case None => List()
        }
      }
    }
  }

  override def toString = {
    name
  }
}


class Player(val agent : Agent) {
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

abstract class TargetedEffect[TargetType] {
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
