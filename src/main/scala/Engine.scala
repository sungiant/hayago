package hayago

import cats._
import cats.std.all._
import cats.syntax.eq._

object Engine {
  case class Turn (action: Either[Unit, Location])
  object Turn {
    val pass = Turn (Left (()))
    def play (x: Int, y: Int) = Turn (Right (Location (x, y)))
  }

  sealed trait Player
  object Montague extends Player
  object Capulet extends Player

  sealed trait Colour
  object Black extends Colour
  object White extends Colour

  case class Location (x: Int, y: Int)

  case class GameConfiguration (boardSize: Int, firstTurn: Player, handicap: Map[Location, Player], komi: Double)

  case class GameState (setup: GameConfiguration, history: List[Turn]) {
    def boardState: List[Option[Colour]] = (0 until setup.boardSize * setup.boardSize).map { i =>
      val x = i % setup.boardSize
      val y = (i - x) / setup.boardSize
      setup.handicap.get (Location (x, y)).map {
        case player if player == setup.firstTurn => Some (Black)
        case _ => Some (White)
      }.getOrElse (None)
    }.toList

    def boardState2DA: Array[Array[Option[Colour]]] = ???

    def isComplete: Boolean = history.reverse match {
      case a :: b :: _ => (a, b) match {
        case (Turn.pass, Turn.pass) => true
        case _ => false
      }
      case _ => false
    }
  }

}