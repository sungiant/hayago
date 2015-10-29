package hayago

object Game {
  import scala.util._
  import com.github.nscala_time.time.Imports._
  import scala.collection.immutable.HashSet
  import cats.data.{ReaderT, Reader, Kleisli}
  import cats.Id
  import cats.Traverse.ops._

  object Logic {

    //def hsEqual[T] (a: HashSet[T], b: HashSet[T]): Boolean = {
    //  val tests = a.map (b.contains(_)).toList ::: b.map (a.contains(_)).toList
    //  tests.filterNot (_ == true).size == 0
    //}

    // this fn doesn't care about who's move it actually it.
    // it just takes a board, a point and a colour, and updates the board state according to the rules of go.
    def applyPlay (i: Board.Intersection, colour: Colour): ReaderT[Try, Board, Board] = Kleisli { board: Board =>
      ???
      println (s"applyPlay ~ intersection:$i, colour:$colour")
      val opposingColour = Colour.opposition (colour)
      val opposingGroups = board.groups (opposingColour)
      println (s"applyPlay ~ num opposing groups:${opposingGroups.size}")
      board (i) match {
        case Success (None) => opposingGroups
          .map (g => (g, g.liberties.run (board)))
          .toMap
          .foldLeft (Try (Map.empty[Board.Group, HashSet[Board.Intersection]])) { case (a, (k, v)) => for { aa <- a; vv <- v } yield aa + (k -> vv) }
          .flatMap (_.foldLeft (Try (board)) { case (a, (g, l)) => for { aa <- a; bb <- aa.cleared (l) } yield bb })
          .flatMap (_.add (i, colour))
        case _ => Try (throw new Exception)
      }
    }
  }

  final case class Board (size: Int, private val grid: Matrix[Option[Colour]]) {

    if (grid.columnCount != size || grid.rowCount != size) throw new Exception

    def apply (i: Board.Intersection): Try[Option[Colour]] =
      Try { grid (i.x, i.y) }

    def apply (s: String): Try[Option[Colour]] =
      Try { Game.Board.Intersection.unapply (s).get }.flatMap (apply)

    def add (i: Board.Intersection, colour: Colour): Try[Board] =
      updated (i, Some (colour))

    def updated (i: Board.Intersection, state: Option[Colour]): Try[Board] =
      Try { Board (size, grid.updated (i.x, i.y, state)) }

    def cleared (i: Board.Intersection): Try[Board] =
      updated (i, None)

    def cleared (hs: HashSet[Board.Intersection]): Try[Board] =
      hs.foldLeft (Try (this)) { (a, i) => a.flatMap (_.cleared (i)) }

    def stoneLocations: HashSet[Board.Intersection] = HashSet {
      grid
        .zipWithAxes
        .collect { case (Some (value), x, y) => Board.Intersection (x, y) }
        .distinct
        : _*
    }

    def stones: Map[Board.Intersection, Colour] =
      grid
        .zipWithAxes
        .collect { case (Some (value), x, y) => (Board.Intersection (x, y), value) }
        .toMap

    def groups: HashSet[Board.Group] = {
      //println (s"groups ~ board:$this")
      def grow (colour: Colour, locations: HashSet[Board.Intersection]): HashSet[Board.Intersection] = {
        println (s"groups.grow ~ colour:$colour locactions:$locations")
        val locations2 = HashSet {
          locations.flatMap { i =>
            (i.neighbours.run (this).get + i) //todo: refactor out `get` call
              .map (ix => (ix, apply (ix)))
              .collect { case (ix, Success (Some (c))) if c == colour => ix }
          }
          .toList
          .distinct
          : _*
        }
        if (locations == locations2) locations2
        else grow (colour, locations2)
      }

      HashSet {
        stones
          .map { case (i, c) =>
            val hs = HashSet (i :: Nil: _*)
            val hs2 = grow (c, hs)
            Board.Group (c, hs2)
          }
          .toList
          .distinct
          : _*
      }
    }

    def groups (colour: Colour): HashSet[Board.Group] = groups.filter (g => g.colour == colour)
  }
  object Board {

    def createS (size: Int, data: Map [String, Colour]): Board = {
      println (s"createS ~ size:$size, data:$data")
      val data2: Map [Board.Intersection, Colour] = Try { data.collect { case (Board.Intersection (i), c) => (i, c) }.toMap }.getOrElse(Map())
      create (size, data2)
    }

    def create (size: Int, data: Map [Board.Intersection, Colour]): Board = {
      println (s"create ~ size:$size, data:$data")
      val g = Matrix.tabulate[Option[Colour]] (size, size) { (x, y) =>
        data.get (Board.Intersection (x, y))
      }
      Board (size, g)
    }

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
        None
        /*
        import scala.util.matching.Regex._
        import scala.util._
        object int { def unapply (str: String): Option[Int] = Try(str.toInt).toOption }
        object char { def unapply (str: String): Option[Char] = str.headOption }

        "([A-Za-z])([0-9]{1,2})".r.findFirstMatchIn (str) match {
          case Some (Groups (char (c), int (n))) => Some (Intersection (c.toInt - 1, n - 1))
          case _ => None
        }
        */
      }
    }

    final case class Group (colour: Colour, locations: HashSet[Board.Intersection]) {
      def isValid: Reader[Board, Boolean] = Reader { board: Board => board.groups.contains (this) }

      // Given a board, if this group exists on that board, returns the set of neighbouring connected `valid` intersections.
      def neighbours: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        println ("Intersection.neighbours")
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
        println ("Intersection.liberties")
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
        println ("Intersection.connections")
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
  object Signal {
    object Pass extends Signal
    object Resign extends Signal
  }
  case class Turn (action: Either[Signal, Board.Intersection], time: DateTime = DateTime.now)
  object Turn {
    val pass = Turn (Left (Signal.Pass))
    val resign = Turn (Left (Signal.Resign))
    def play (s: String): Turn = Board.Intersection.unapply (s).map (play).getOrElse (pass)
    def play (i: Board.Intersection): Turn = Turn (Right (i))
  }

  sealed trait Player
  object Player {
    def opposition (player: Player) = player match {
      case Montague => Capulet
      case Capulet => Montague
    }
    object Montague extends Player // Man
    object Capulet extends Player  // Computer
  }

  sealed trait Colour
  object Colour {
    def opposition (colour: Colour) = colour match {
      case Black => White
      case White => Black
    }
    object Black extends Colour
    object White extends Colour
  }

  val firstTurnColour = Colour.Black // Black is always first

  case class Configuration (boardSize: Int = 19, firstTurn: Player = Player.Montague, handicap: Map[Board.Intersection, Player] = Map (), komi: Float = 6.5f)

  case class State (setup: Configuration = Configuration (), history: List[Turn] = Nil) {
    def board: Board = {
      val n = setup.boardSize * setup.boardSize
      val grid = Matrix.tabulate[Option[Colour]] (n, n) { (_, _) => None }
      val empty = Board (n, grid)
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
    def player (colour: Colour) = if (colour == Colour.Black) setup.firstTurn else Player.opposition (setup.firstTurn)

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
