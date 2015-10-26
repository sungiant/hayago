package hayago

import hayago.Game.Board.Intersection

object Game {
  import scala.util._
  import com.github.nscala_time.time.Imports._
  import scala.collection.immutable.HashSet

  object Logic {
    case class Group (colour: Colour, stones: HashSet[Board.Intersection])

    def liberties (board: Board, group: Group): HashSet[Board.Intersection] = {
      HashSet() ++ group.stones.toList.flatMap { i =>
        (i + Intersection (0, 1) :: i + Intersection (1, 0) :: i + Intersection (0, -1) :: i + Intersection (-1, 0) :: Nil)
          .map (ix => (ix, board (ix)))
          .collect { case (ix, Success (None)) => ix }
      }.distinct
    }

    def aggregate (board: Board, colour: Colour): HashSet[Group] = {
      ???
    }
    // this fn doesn't care about who's move it actually it.
    // it just takes a board, a point and a colour, and updates the board state according to the rules of go.
    def applyPlay (board: Board, i: Board.Intersection, colour: Colour): Try[Board] = {
      val enemyGroups = aggregate (board, Colour.opposition (firstTurnColour))
      val toRemove = HashSet() ++ enemyGroups.toList.collect { case enemyGroup if
      liberties (board, enemyGroup).contains(i) => enemyGroup.stones.toList
      }.flatten
      for {
        b1 <- board.cleared (toRemove)
        b2 <- b1.add (i, colour)
        fg = aggregate (b2, colour)
        r2 = HashSet() ++ fg.toList.collect { case fg if liberties (b2, fg).isEmpty => fg.stones.toList }.flatten
        b3 <- b2.cleared (r2)
      } yield b3
    }
  }

  final case class Board (private val grid: Matrix[Option[Colour]]) {
    val size: Int = grid.rowCount
    // get
    def apply (i: Board.Intersection): Try[Option[Colour]] =
      Try { grid (i.x, i.y) }

    def apply (s: String): Try[Option[Colour]] =
      Try { Game.Board.Intersection.unapply (s).get }.flatMap (apply)

    def add (i: Board.Intersection, colour: Colour): Try[Board] =
      updated (i, Some (colour))

    // updated
    def updated (i: Board.Intersection, state: Option[Colour]): Try[Board] =
      Try { Board (grid.updated (i.x, i.y, state)) }

    // cleared
    def cleared (i: Board.Intersection): Try[Board] =
      updated (i, None)

    def cleared (hs: HashSet[Board.Intersection]): Try[Board] =
      hs.foldLeft (Try (this)) { (a, i) => a.flatMap (_.cleared (i)) }

    def stones: HashSet[Board.Intersection] =
      HashSet () ++ grid.zipWithAxes.collect { case (Some (value), x, y) => Board.Intersection (x, y) }
  }
  object Board {
    // counting from zero
    case class Intersection (x: Int, y: Int) {
      override def toString = if (x < 26) ('A'.toInt + x).toChar + (y + 1).toString else toString
      def + (i: Intersection) = Intersection (x + i.x, y + i.y)
      def - (i: Intersection) = Intersection (x - i.x, y - i.y)
    }
    object Intersection {
      def unapply (str: String): Option[Intersection] = {
        import scala.util.matching.Regex._
        import scala.util._
        object int { def unapply (str: String): Option[Int] = Try(str.toInt).toOption }
        object char { def unapply (str: String): Option[Char] = str.headOption }
        "([A-Za-z])([0-9]{1,2})".r.findFirstMatchIn (str) match {
          case Some (Groups (char (c), int (n))) => Some (Intersection (c.toInt - 1, n - 1))
          case _ => None
        }
      }
    }
  }

  sealed trait Signal
  object Pass extends Signal
  object Resign extends Signal
  case class Turn (action: Either[Signal, Board.Intersection], time: DateTime = DateTime.now)
  object Turn {
    val pass = Turn (Left (Pass))
    val resign = Turn (Left (Resign))
    def play (s: String): Turn = Board.Intersection.unapply (s).map (play).getOrElse (pass)
    def play (i: Board.Intersection): Turn = Turn (Right (i))
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

  case class Configuration (boardSize: Int = 19, firstTurn: Player = Montague, handicap: Map[Board.Intersection, Player] = Map (), komi: Float = 6.5f)

  case class State (setup: Configuration = Configuration (), history: List[Turn] = Nil) {
    def board: Board = {
      val n = setup.boardSize * setup.boardSize
      val grid = Matrix.tabulate[Option[Colour]] (n, n) { (_, _) => None }
      val empty = Board (grid)
      val withHandicap = setup.handicap.toList.foldLeft (empty) { (a, i) =>
        val intersection = i._1
        val player = i._2
        Logic.applyPlay (a, intersection, colour (player)).get
      }
      history.indices.foldLeft (withHandicap) { (a, i) =>
        val turn = history (i)
        val colour = colourToPlayTurn (i)
        turn.action match {
          case Left (_) => a
          case Right (intersection) => Logic.applyPlay (a, intersection, colour).get
        }
      }
    }
    def playerToPlayTurn (index: Int): Player = if (index % 2 == 0) setup.firstTurn else Player.opposition (setup.firstTurn)
    def playerToPlayNext = playerToPlayTurn (history.size)
    def colourToPlayTurn (index: Int): Colour = if (playerToPlayTurn (index) == setup.firstTurn) firstTurnColour else Colour.opposition (firstTurnColour)
    def colourToPlayNext = colourToPlayTurn (history.size)
    def colour (player: Player) = if (player == setup.firstTurn) firstTurnColour else Colour.opposition (firstTurnColour)
    def player (colour: Colour) = if (colour == Black) setup.firstTurn else Player.opposition (setup.firstTurn)

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
