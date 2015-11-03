package hayago.game

import hayago._
import scala.util._
import scala.collection.immutable.HashSet

final case class State (setup: Configuration, history: List[Turn] = Nil) {
  def startBoard = setup
    .handicap
    .toList
    .foldLeft (Board.create (setup.boardSize)) { (a, i) =>
      val intersection = i._1
      val player = i._2
      a.applyPlay (intersection, colour (player)).get
    }

  def board: Board = history
    .zipWithIndex
    .foldLeft (startBoard) { case (acc, (turn, i)) =>
      turn.action match {
        case Left (_) => acc
        case Right (intersection) => acc.applyPlay (intersection, colourToPlayTurn (i)).get
      }
    }

  private def allBoards : List[Board] = history
    .zipWithIndex
    .foldLeft (startBoard :: Nil) { case (acc, (turn, i)) =>
      turn.action match {
        case Left (_) => acc
        case Right (intersection) =>
          acc :+ acc.last.applyPlay (intersection, colourToPlayTurn (i)).get
      }
    }

  def playerToPlayTurn (index: Int): Player =
    if (index % 2 == 0) setup.firstTurn else Player.opposition (setup.firstTurn)

  def playerToPlayNext =
    playerToPlayTurn (history.size)

  def colourToPlayTurn (index: Int): Colour =
    if (playerToPlayTurn (index) == setup.firstTurn) firstTurnColour else firstTurnColour.opposition

  def colourToPlayNext =
    colourToPlayTurn (history.size)

  def colour (player: Player) =
    if (player == setup.firstTurn) firstTurnColour else firstTurnColour.opposition

  def player (colour: Colour) =
    if (colour == Colour.Black) setup.firstTurn else Player.opposition (setup.firstTurn)

  def applyTurn (turn: Turn): Try[State] = isComplete match {
    case true => Failure[State](State.GameOverException)
    case false => turn.action match {
      case Right (i) => board.applyPlay (i, colourToPlayNext) match {
        case Success (boardWithPlay) =>
          HashSet (allBoards: _*).contains (boardWithPlay) match {
            case true => Failure[State](State.IllegalMoveDueToKoException)
            case false => Success (State (setup, history :+ turn))
          }
        case Failure (f) => Failure[State](f)
      }
      case Left (_) => Success (State (setup, history :+ turn))
    }
  }

  def isTurnLegal (turn: Turn): Boolean = applyTurn (turn) match {
    case Success (_) => true
    case Failure (_) => false
  }

  def isComplete: Boolean = history.collect { case t @ Turn (Left (Signal.Resign), _) => () }.isEmpty match {
    case false => true
    case true => history.reverse match {
      case first :: second :: _ => (first, second) match {
        case (Turn (Left (Signal.Pass), _), Turn (Left (Signal.Pass), _)) => true
        case _ => false
      }
      case _ => false
    }
  }
}
object State {
  object IllegalMoveDueToKoException extends Exception
  object GameOverException extends Exception
}