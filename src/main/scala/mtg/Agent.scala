package mtg

abstract class Agent {
  /* Called in setup to get the card list of this agent */
  def deck : List[Card]

  /* Called at start of game. TODO: Mulligans */
  def receiveHand(hand : Hand)

  /* Main game loop, called every time this agent receives priority */
  def receivePriority(state : PublicGameState, me : PrivateGameState) : List[Action]
}
class DumbAgent(name :String, noOfMountains : Int) extends Agent {
  override def deck : List[Card] = {
    val mountains : List[Card] = (0 until noOfMountains).map { (i) => new Mountain }.toList
    val bolts : List[Card] = (0 until 60 - noOfMountains).map { (i) => new LightningBolt }.toList

    mountains ++ bolts
  }

  override def receiveHand(hand : Hand) {
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
                spell match {
                  case castable : Castable =>
                    List(new Action.Cast(
                      castable,
                      (target : Target) => {
                        target match {
                          case p : Target.Player => me.player
                        }
                      },
                      () => List(new ManaPayment[Red](1)),
                      () => List(new Action.ActivateManaAbility(mountain.abilities.head)),
                      () => List(new Action.RemoveManaFromPool[Red](1))))
                }
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
