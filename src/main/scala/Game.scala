package hayago

object Game {
  import scala.util._
  import com.github.nscala_time.time.Imports._
  import scala.collection.immutable.HashSet
  import cats.data.{ReaderT, Reader, Kleisli}
  import cats.Id
  import cats.Traverse.ops._

  object Logic {
    // this fn doesn't care about who's move it actually it.
    // it just takes a board, an intersection and a colour, and trys to update the board state according
    // to the rules of Go, fails if the move is illegal.
    def applyPlay (i: Board.Intersection, colour: Colour): ReaderT[Try, Board, Board] = Kleisli { board: Board =>
      //println (s"applyPlay ~ intersection:$i, colour:$colour")
      val opposingColour = Colour.opposition (colour)
      board.add (i, colour) match {
        case Success (boardWithPlay) =>
          val opposingGroups = boardWithPlay.groups (opposingColour)
          val e = Map.empty[Board.Group, HashSet[Board.Intersection]]
          //println (s"applyPlay ~ num opposing groups:${opposingGroups.size}")
          opposingGroups
            .map (group => (group, group.liberties.run (boardWithPlay)))
            .foldLeft (Try (e)) { case (accT, (group, libertiesT)) => for { accBoard <- accT; liberties <- libertiesT } yield accBoard + (group -> liberties) }
            .map (_.toList.collect { case (g, l) if l.size == 0 => (g, l) }.toMap)
            .flatMap (_.foldLeft (Try (boardWithPlay)) { case (accT, (group, liberties)) => for { accBoard <- accT; newBoard <- accBoard.cleared (group.locations) } yield newBoard })

        case _ => Failure[Board] (new Exception)
      }
    }
  }

  final case class Board (size: Int, private val grid: Matrix[Option[Colour]]) {

    if (grid.columnCount != size || grid.rowCount != size) throw new Exception

    def apply (i: Board.Intersection): Try[Option[Colour]] =
      Try { grid.apply(i.x, i.y) }

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

    def stoneLocations: HashSet[Board.Intersection] = {
      val result = HashSet () ++ grid
        .zipWithAxes
        .collect { case (Some (value), x, y) => Board.Intersection (x, y) }
      //println (s"Board.stoneLocations:${result.size}")
      result
    }

    def stones: Map[Board.Intersection, Colour] = {
      val result = grid
        .zipWithAxes
        .collect { case (Some(value), x, y) => (Board.Intersection(x, y), value) }
        .toMap
      //println (s"Board.stones:${result.size}")
      result
    }


    def groups: HashSet[Board.Group] = {
      import scala.annotation.tailrec
      @tailrec def grow (colour: Colour, locations: HashSet[Board.Intersection]): HashSet[Board.Intersection] = {
        //println (s"Board.groups.grow ~ colour:$colour locations:$locations")
        val locations2: HashSet[Board.Intersection] = HashSet () ++ locations.flatMap { i =>
          (i.neighbours.run (this).get + i) //todo: refactor out `get` call
            .map (ix => (ix, apply (ix)))
            .collect { case (ix, Success (Some (c))) if c == colour => ix }
        }

        if (locations == locations2) locations2
        else grow (colour, locations2)
      }

      //println (s"Board.groups")
      HashSet () ++ stones
        .map { case (i, c) =>
          //println (s"Board.groups stone: $i")
          val hs = HashSet () + i
          val hs2 = grow (c, hs)
          Board.Group (c, hs2)
        }
    }

    def groups (colour: Colour): HashSet[Board.Group] = {
      //println (s"groups (colour:$colour)")
      groups.filter (g => g.colour == colour)
    }
  }
  object Board {

    def create (size: Int, data: Map [Board.Intersection, Colour]): Board = {
      //println (s"create ~ size:$size, data:$data")
      val g = Matrix.tabulate[Option[Colour]] (size, size) { (x, y) =>
        data.get (Board.Intersection (x, y))
      }
      Board (size, g)
    }

    def create (size: Int): Board = {
      //println (s"create ~ size:$size")
      val g = Matrix.tabulate[Option[Colour]] (size, size) { (_, _) => None }
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
        board (this).map { _ => HashSet () ++ (north :: east :: south :: west :: Nil)
          .map { i => (i, board (i)) }
          .collect { case (i, Success (x)) => i }
        }
      }
    }
    object Intersection {
      def unapply (str: String): Option[Intersection] = {
        import scala.util.matching.Regex._
        object int { def unapply (str: String): Option[Int] = Try (str.toInt).toOption }
        object char { def unapply (str: String): Option[Char] = str.toUpperCase.headOption  }
        "([A-Za-z])([0-9]{1,2})".r.findFirstMatchIn (str) match {
          case Some (Groups (char (c), int (n))) =>
            Some (Intersection (c.toInt - 'A'.toInt, n - 1))
          case _ => None
        }
      }
    }

    final case class Group (colour: Colour, locations: HashSet[Board.Intersection]) {
      def isValid: Reader[Board, Boolean] = Reader { board: Board => board.groups.contains (this) }

      // Given a board, if this group exists on that board, returns the set of neighbouring connected `valid` intersections.
      def neighbours: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        //println ("Intersection.neighbours")
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
        //println ("Intersection.liberties")
        neighbours
          .run (board)
          .map { n => HashSet () ++ n
            .map { i => (i, board (i)) }
            .collect { case (i, Success (None)) => i }
          }
      }

      // Given a board, if this group exists on that board, returns the set of neighbouring connected intersections occupied by the opposing colour.
      def connections: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        //println ("Intersection.connections")
        neighbours
          .run (board)
          .map { n => HashSet () ++ n
            .map { i => (i, board (i)) }
            .collect { case (i, Success (Some (c))) if c == Colour.opposition (colour) => i }
          }
      }
    }
  }

  sealed trait Signal
  object Signal {
    object Pass extends Signal { override def toString = "PASS" }
    object Resign extends Signal { override def toString = "RESIGN" }
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
    object Montague extends Player { override def toString = "MONTAGUE" } // Man
    object Capulet extends Player { override def toString = "CAPULET" } // Computer
  }

  sealed trait Colour
  object Colour {
    def opposition (colour: Colour) = colour match {
      case Black => White
      case White => Black
    }
    object Black extends Colour { override def toString = "BLACK" }
    object White extends Colour { override def toString = "WHITE" }
  }

  val firstTurnColour = Colour.Black // Black is always first

  case class Configuration (boardSize: Int = 19, firstTurn: Player = Player.Montague, handicap: Map[Board.Intersection, Player] = Map (), komi: Float = 6.5f)

  case class State (setup: Configuration = Configuration (), history: List[Turn] = Nil) {
    def board: Board = {
      val empty = Board.create (setup.boardSize)
      //println (s"State.board ~ empty:${empty.stones.size}")
      val withHandicap = setup.handicap.toList.foldLeft (empty) { (a, i) =>
        val intersection = i._1
        val player = i._2
        Logic.applyPlay (intersection, colour (player)).run (a).get
      }
      //println (s"State.board ~ withHandicap:${withHandicap.stones.size}")
      val withTurns = history
        .zipWithIndex
        .foldLeft (withHandicap) { case (acc, (turn, i)) =>
          //println (s"State.board ~ (i:$i, turn:$turn) ~ acc:${acc.stones.size}")
          val colour = colourToPlayTurn (i)
          turn.action match {
            case Left (_) =>
              //println (s"State.board ~ Shiiiiite")
              acc
            case Right (intersection) =>
              //println (s"State.board ~ Logic.applyPlay (intersection:$intersection, colour:$colour)")
              val applied = Logic.applyPlay (intersection, colour).run (acc).get
              applied
          }
        }
      //println (s"State.board ~ withTurns:${withTurns.stones.size}")
      withTurns
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
