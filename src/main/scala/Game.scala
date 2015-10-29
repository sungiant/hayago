package hayago

import hayago.Game.Board.Intersection

object Game {
  import scala.util._
  import com.github.nscala_time.time.Imports._
  import scala.collection.immutable.HashSet
  import cats.data.{ReaderT, Reader, Kleisli}
  import cats.Id
  import cats.Traverse.ops._

  object Logic {

    // this fn doesn't care about who's move it actually it.
    // it just takes a board, a point and a colour, and updates the board state according to the rules of go.
    def applyPlay (i: Board.Intersection, colour: Colour): ReaderT[Try, Board, Board] = Kleisli { board: Board =>
      val opposingColour = Colour.opposition (colour)
      val opposingGroups = board.groups (opposingColour)

      board (i) match {
        case Success (None) =>
          val newBoard = opposingGroups
            .map (g => (g, g.liberties.run (board)))
            .toMap
            .foldLeft (Try (Map.empty[Board.Group, HashSet[Board.Intersection]])) { case (a, (k, v)) => for { aa <- a; vv <- v } yield aa + (k -> vv) }
            .flatMap (_.foldLeft (Try (board)) { case (a, (g, l)) => for { aa <- a; bb <- aa.cleared (l) } yield bb })
            .flatMap (_.add (i, colour))

          // now need to check for suicide

          newBoard
        case _ =>
          Try (throw new Exception)
      }


      /*
      val toRemove = HashSet() ++ enemyGroups.toList.collect { case enemyGroup if liberties (enemyGroup).run (board).contains(i) =>
        enemyGroup.stones.toList
      }.flatten
      for {
        b1 <- board.cleared (toRemove)
        b2 <- b1.add (i, colour)
        fg = aggregate (b2, colour)
        r2 = HashSet() ++ fg.toList.collect { case fg if liberties (b2, fg).isEmpty => fg.stones.toList }.flatten
        b3 <- b2.cleared (r2)
      } yield b3
      */
    }
  }

  final case class Board (private val grid: Matrix[Option[Colour]]) {
    val size: Int = grid.rowCount

    def apply (i: Board.Intersection): Try[Option[Colour]] =
      Try { grid (i.x, i.y) }

    def apply (s: String): Try[Option[Colour]] =
      Try { Game.Board.Intersection.unapply (s).get }.flatMap (apply)

    def add (i: Board.Intersection, colour: Colour): Try[Board] =
      updated (i, Some (colour))

    def updated (i: Board.Intersection, state: Option[Colour]): Try[Board] =
      Try { Board (grid.updated (i.x, i.y, state)) }

    def cleared (i: Board.Intersection): Try[Board] =
      updated (i, None)

    def cleared (hs: HashSet[Board.Intersection]): Try[Board] =
      hs.foldLeft (Try (this)) { (a, i) => a.flatMap (_.cleared (i)) }

    def stonesLocations: HashSet[Board.Intersection] =
      HashSet () ++ grid.zipWithAxes.collect { case (Some (value), x, y) => Board.Intersection (x, y) }

    def stones: Map[Board.Intersection, Colour] =
      grid.zipWithAxes.collect { case (Some (value), x, y) => (Board.Intersection (x, y), value) }.toMap

    def groups: HashSet[Board.Group] = {
      def f (v: Colour, is: HashSet[Board.Intersection]): HashSet[Board.Intersection] = {
        val is2 = HashSet () ++ is.toList.flatMap { i =>
          (i.north :: i.east :: i.south :: i.west :: Nil)
            .map (ix => (ix, apply (ix)))
            .collect { case (ix, Success (Some (colour))) if colour == v => ix }
        }.distinct
        if (is == is2) is2 else f (v, is2)
      }
      HashSet {
        stones
          .map { case (i, v) => Board.Group (v, f (v, HashSet (i :: Nil: _*))) }
          .toList
          .distinct
          : _*
      }
    }

    def groups (colour: Colour): HashSet[Board.Group] = groups.filter (g => g.colour == colour)
  }
  object Board {
    // counting from zero
    final case class Intersection (x: Int, y: Int) {
      override def toString = if (x < 26) ('A'.toInt + x).toChar + (y + 1).toString else toString
      def + (i: Intersection) = Intersection (x + i.x, y + i.y)
      def - (i: Intersection) = Intersection (x - i.x, y - i.y)
      def north = this + Intersection (0, 1)
      def east = this + Intersection (1, 0)
      def south = this + Intersection (0, -1)
      def west = this + Intersection (-1, 0)

      // Given a board, if this intersection exists on that board, returns the set of neighbouring connected `valid` intersections.
      def neighbours: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
        board (this).map { _ =>
          HashSet {
            (north :: east :: south :: west :: Nil)
              .map { i => (i, board (i)) }
              .collect { case (i, Success (x)) => i }
              .distinct
              : _*
          }
        }
      }
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

    final case class Group (colour: Colour, locations: HashSet[Board.Intersection]) {
      def isValid: Reader[Board, Boolean] = Reader { board: Board => board.groups.contains (this) }

      // Given a board, if this group exists on that board, returns the set of neighbouring connected `valid` intersections.
      def neighbours: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        isValid
          .run (board) match {
          case false => Try (throw new Exception)
          case true => locations
            .map (_.neighbours.run (board))
            .reduce { (a, b) => for {aa <- a; bb <- b} yield aa ++ bb }
        }
      }

      // Given a board, if this group exists on that board, returns the set of neighbouring connected `valid` intersections.
      def liberties: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        neighbours
          .run (board)
          .map { n =>
            HashSet { n
              .map { i => (i, board (i)) }
              .collect { case (i, Success (None)) => i }
              .toList
              .distinct
              : _*
            }
          }
      }

      // Given a board, if this group exists on that board, returns the set of neighbouring connected intersections occupied by the opposing colour.
      def connections: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        neighbours
          .run (board)
          .map { n =>
            HashSet { n
              .map { i => (i, board (i)) }
              .collect { case (i, Success (Some (c))) if c == Colour.opposition (colour) => i }
              .toList
              .distinct
              : _*
            }
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
        Logic.applyPlay (intersection, colour (player)).run (a).get
      }
      history.indices.foldLeft (withHandicap) { (a, i) =>
        val turn = history (i)
        val colour = colourToPlayTurn (i)
        turn.action match {
          case Left (_) => a
          case Right (intersection) => Logic.applyPlay (intersection, colour).run (a).get
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
