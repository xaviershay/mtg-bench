package mtg

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

trait Castable extends Card {
  def makeSpell(targets : List[Targetable], payments : List[Action.Payment]) : Spell
}

trait Interceptor
trait ColorInterceptor extends Interceptor {
  def interceptColor(existingColors : List[Color]) : List[Color]
}

trait AbilityInterceptor extends Interceptor {
  def interceptAbilities(existingAbilities : List[Ability]) : List[Ability]
}

class LightningBolt extends Card with Castable {
  class LightningBoltSpell(
    source : Card,
    targets : List[Targetable],
    payments : List[Action.Payment]
  ) extends Spell(source, targets, payments) {
    def execute(state : GameState) : List[Effect] = {
      targets.map( (player) => player match {
        case p : Player => new Effect.DamageToPlayer(p, 3)
      })
    }
  }
  override val baseColors = List(Red)
  override val baseTypes  = List(Instant)
  override def resolve(state : GameState) = {
    controller match {
      case Some(player) => List(new Effect.DamageToPlayer(player, 3))
      case None => List()
    }
  }
  override def makeSpell(targets : List[Targetable], payments : List[Action.Payment]) = {
    new LightningBoltSpell(this, targets, payments)
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

