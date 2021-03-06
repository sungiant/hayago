package hayago.game

import scala.util._
import scala.collection.immutable.HashSet
import cats.instances.option._
import cats.instances.int._
import cats.syntax.eq._
import cats.data.{Reader, Kleisli}
import scala.collection.immutable.HashSet

/**
 * The session data encapsulates the entire setup and history of a (finished or unfinished) game.  The
 * functions provide only hard facts about the game, this part of the code is not intelligent, it doesn't
 * for example have any level opinion about dead stones.
 */
final case class Session (setup: Configuration, history: List[Turn] = Nil) {
  /**
   * The initial board state, accounting for handicaps, before any play has taken place.
   */
  lazy val startBoard = setup
    .handicap
    .toList
    .foldLeft (Board.create (setup.boardSize)) { (a, i) =>
      val intersection = i._1
      val player = i._2
      a.applyMove (intersection, colour (player)).get
    }

  lazy val currentBoard: Board = history
    .zipWithIndex
    .foldLeft (startBoard) { case (acc, (turn, i)) =>
      turn.action match {
        case Left (_) => acc
        case Right (intersection) => acc.applyMove (intersection, Session.colourToPlayTurn (i).run (setup)).get
      }
    }

  private lazy val allBoards: List[Board] = history
    .zipWithIndex
    .foldLeft (startBoard :: Nil) { case (acc, (turn, i)) =>
      turn.action match {
        case Left (_) => acc
        case Right (intersection) =>
          acc :+ acc.last.applyMove (intersection, Session.colourToPlayTurn (i).run (setup)).get
      }
    }

  /**
   * Returns the player that has the next turn.
   */
  lazy val playerToPlay = Session.playerToPlayTurn (history.size).run (setup)

  /**
   * Returns the colour that will be played on the next turn.
   */
  lazy val colourToPlay = Session.colourToPlayTurn (history.size).run (setup)

  /**
   * Given a player, returns that player's colour.
   */
  def colour (player: Player) = if (player === setup.firstTurn) firstTurnColour else firstTurnColour.opposition

  /**
   * Given a colour, returns that colour's player.
   */
  def player (colour: Colour) = if (colour === Colour.Black) setup.firstTurn else Player.opposition (setup.firstTurn)

  /**
   * Number of captures so far for the given colour.
   */
  def captureCount (colour: Colour): Int = {
    val m = 2
    val r = colour.opposition match {
      case Colour.Black => 0
      case Colour.White => 1
    }

    val numPlays = history
      .zipWithIndex
      .filter (_._2 % m === r)
      .map (_._1)
      .collect { case Turn (Right (i), _) => i }
      .size

    val numOnBoard = currentBoard.stoneCount (colour.opposition)

    numPlays - numOnBoard
  }

  /**
   * The komi for the given colour.
   */
  def komi (colour: Colour): Float = if (colour === firstTurnColour) 0f else setup.komi

  /**
   * Given a list of stone locations on the current board that contain stones that
   * are considered to be dead, this function scores each player.
   */
  def score (deadStones: HashSet[Intersection]): Try[Map[Player, Score]] = {
    currentBoard.without (deadStones).map { finalBoard =>
      val players = Player.Capulet :: Player.Montague :: Nil
      players.map { p =>
        def s (colour: Colour) = {
          val k = komi (colour)
          val t = finalBoard.territories
            .map (z => (z, z.controlledBy.run (finalBoard).get))
            .filter (_._2 === Some (colour))
            .map (_._1)
            .toList
            .map (z => z.locations.size)
            .sum
          val c = captureCount (colour) + currentBoard.stoneCount (colour.opposition) - finalBoard.stoneCount (colour.opposition)

          val h = deadStones.toList.map (i => currentBoard (i).get).count (_ === Some (colour.opposition))

          Score (k, t, c - h, h)
        }
        (p, s (colour (p)))
      }.toMap
    }
  }

  def applyTurn (turn: Turn): Try[Session] = (isComplete, isAgreementPhase) match {
    case (true, _) => Failure[Session](IllegalTurnException (IllegalTurnReason.GameComplete))
    case (_, true) => turn.action match {
      case Left (Signal.Evaluation (_)) => Success (Session (setup, history :+ turn))
      case _ => Failure[Session](IllegalTurnException (IllegalTurnReason.EvaluationExpected))
    }
    case _ => turn.action match {
      case Left (_) => Success (Session (setup, history :+ turn))
      case Right (i) => currentBoard.applyMove (i, colourToPlay) match {
        case Failure (f) => Failure[Session](f)
        case Success (boardWithPlay) =>
          HashSet (allBoards: _*).contains (boardWithPlay) match {
            case true => Failure[Session](IllegalTurnException (IllegalTurnReason.Ko))
            case false => Success (Session (setup, history :+ turn))
          }
      }
    }
  }

  def isTurnLegal (turn: Turn): Boolean = applyTurn (turn) match {
    case Success (_) => true
    case Failure (_) => false
  }

  lazy val isAgreementPhase: Boolean = history.collect { case t @ Turn (Left (Signal.Resign), _) => () }.isEmpty match {
    case false => false
    case true => history.reverse match {
      case first :: second :: third :: tail => (first, second, third) match {
        // the last two moves were passes
        case (Turn (Left (Signal.Pass), _), Turn (Left (Signal.Pass), _), _) => true
        // the last three moves were an evaluation and then two passes
        case (Turn (Left (Signal.Evaluation (_)), _), Turn (Left (Signal.Pass), _), Turn (Left (Signal.Pass), _)) => true
        case _ => false
      }
      case first :: second :: Nil => (first, second) match { // A game with only two moves (as if it gets here not caught by the above)!
        // the last two moves were passes
        case (Turn (Left (Signal.Pass), _), Turn (Left (Signal.Pass), _)) => true
        case _ => false
      }  
      case _ => false
    }
  }

  // A session is complete if after both players have passed sequentially, each have then submitted equal evaluations
  // of the state of dead stones on the board. Or if someone resigned.
  lazy val isComplete: Boolean = history.collect { case Turn (Left (Signal.Resign), _) => () }.isEmpty match {
    case false => true
    case true => history.reverse match {
      case first :: second :: _ => (first, second) match {
        case (Turn (Left (Signal.Evaluation (ds1st)), _), Turn (Left (Signal.Evaluation (ds2nd)), _)) => ds1st == ds2nd
        case _ => false
      }
      case _ => false
    }
  }

  // Works out all of the possible legal things the next player to play in this session could choose to do next.
  lazy val possibleTurns: List[Turn] = (isComplete, isAgreementPhase) match {
    // If the session is complete, then no more moves are allow.
    case (true, _) => Nil
    // If the session is in the agreement phase then the only legal turn is an evaluation
    // of dead stones.
    case (_, true) => Turn.create (Signal.Evaluation (HashSet ())) :: Nil
    // Otherwise there are plenty of options, including passing.
    case _ => currentBoard
      .legalNextMovesFor (colourToPlay)
      .toList
      .map (i => Turn.create (i)) ::: Turn.create (Signal.Pass) :: Nil
  }

  lazy val result: Option[Map[Player, Score]] = isComplete match {
    case false => None
    case true => history.reverse.headOption match {
      case Some (Turn (Left (Signal.Evaluation (ds)), _)) => score (ds).toOption
      case _ => None
    }
  }
}
object Session {

  // This might be too simple when it comes to resolving disputes in the agreement phase.
  // I think in that scenario the play order can be changed...
  // Will stick to this for now.
  def playerToPlayTurn (index: Int): Reader[Configuration, Player] = Kleisli { setup: Configuration =>
    if (index % 2 === 0) setup.firstTurn else Player.opposition (setup.firstTurn)
  }

  def colourToPlayTurn (index: Int): Reader[Configuration, Colour] = playerToPlayTurn (index).flatMap { playerToPlayTurn =>
    Kleisli { setup: Configuration =>
      if (playerToPlayTurn === setup.firstTurn) firstTurnColour else firstTurnColour.opposition
    }
  }
}
