package hayago

object Engine {
  import cats._
  import cats.std.all._
  import cats.syntax.eq._
  import scala.concurrent.Future
  import cats.state._
  import com.github.nscala_time.time.Imports._

  sealed trait Signal
  object Pass extends Signal
  object Resign extends Signal
  case class Turn (action: Either[Signal, Point], time: DateTime = DateTime.now)
  object Turn {
    val pass = Turn (Left (Pass))
    val resign = Turn (Left (Resign))
    def play (x: Int, y: Int) = Turn (Right (Point (x, y)))
    def play (location: Point) = Turn (Right (location))
  }

  sealed trait Player
  object Player {
    def opposition (player: Player) = player match {
      case Montague => Capulet
      case Capulet => Montague
    }
  }
  object Montague extends Player // Man
  object Capulet extends Player  // Computer

  sealed trait Colour
  object Colour {
    def opposition (colour: Colour) = colour match {
      case Black => White
      case White => Black
    }
  }
  object Black extends Colour
  object White extends Colour

  val firstTurnColour = Black // Black is always first


  def takeTurn () (implicit MF: Monad[Future]): StateT[Future, GameState, Unit] = {
    def ms(implicit MF: Monad[Future]) = MonadState[({type ST[X, Y] = StateT[Future, X, Y]})#ST, GameState]
    for {
      gameState <- ms.get
      _ <- gameState.isComplete match {
        case true => StateT.pure[Future, GameState, Unit] (())
        case false => for {
          _ <- ms.set(gameState.copy(history = gameState.history :+ Turn.pass))
        } yield ()
      }
    } yield ()
  }

  // counting from zero
  case class Point (x: Int, y: Int) {
    override def toString = if (x < 26) ('A'.toInt + x).toChar + (y + 1).toString else toString
  }
  object Point {
    def unapply (str: String): Option[Point] = {
      import scala.util.matching._
      import scala.util.matching.Regex._
      import scala.util._
      object int { def unapply (str: String): Option[Int] = Try(str.toInt).toOption }
      object char { def unapply (str: String): Option[Char] = str.headOption }
      "([A-Za-z])([0-9]{1,2})".r.findFirstMatchIn (str) match {
        case Some(Groups(char(c), int(n))) => Some(Point(c.toInt - 1, n - 1))
        case _ => None
      }
    }
  }

  case class GameConfiguration (boardSize: Int = 19, firstTurn: Player = Montague, handicap: Map[Point, Player] = Map (), komi: Float = 6.5f)

  case class GameState (setup: GameConfiguration, history: List[Turn]) {
    def boardState: List[Option[Colour]] = (0 until setup.boardSize * setup.boardSize).map { i =>
      val x = i % setup.boardSize
      val y = (i - x) / setup.boardSize
      setup.handicap.get (Point (x, y)).map {
        case player if player == setup.firstTurn => Some (Black)
        case _ => Some (White)
      }.getOrElse (None)
    }.toList

    def boardState2DA: Array[Array[Option[Colour]]] = ???

    def playerToPlay: Player = if (history.size % 2 == 0) setup.firstTurn else Player.opposition (setup.firstTurn)
    def colourToPlay: Colour = if (playerToPlay == setup.firstTurn) firstTurnColour else Colour.opposition (firstTurnColour)

    def isTurnLegal (turn: Turn): Boolean = ???

    def isComplete: Boolean = history.reverse match {
      case z if z.contains (Turn.resign) => true
      case first :: second :: _ => (first, second) match {
        case (Turn.pass, Turn.pass) => true
        case _ => false
      }
      case _ => false
    }
  }

}
