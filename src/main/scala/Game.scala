package hayago

object Game {
  import scala.util._
  import com.github.nscala_time.time.Imports._
  import scala.collection.immutable.HashSet
  import cats.data.{ReaderT, Reader, Kleisli}

  val firstTurnColour = Colour.Black // Black is always first

  sealed trait Signal
  object Signal {
    object Pass extends Signal { override def toString = "PASS" }
    object Resign extends Signal { override def toString = "RESIGN" }
    def unapply (s: String): Option[Signal] = s.toLowerCase match {
      case "pass" => Some (Signal.Pass)
      case "resign" => Some (Signal.Resign)
      case _ => None
    }
  }

  sealed trait Player
  object Player {
    def opposition (player: Player) = player match {
      case Montague => Capulet
      case Capulet => Montague
    }
    object Montague extends Player { override def toString = "MONTAGUE" }
    object Capulet extends Player { override def toString = "CAPULET" }
  }

  sealed trait Colour {
    val opposition = this match {
      case Colour.Black => Colour.White
      case Colour.White => Colour.Black
    }
  }
  object Colour {
    object Black extends Colour { override def toString = "BLACK" }
    object White extends Colour { override def toString = "WHITE" }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Turn (action: Either[Signal, Intersection], time: DateTime)
  object Turn {
    def create (i: Intersection): Turn =
      Turn (Right (i), DateTime.now)

    def create (s: Signal): Turn =
      Turn (Left (s), DateTime.now)

    def create (str: String): Turn = Intersection.unapply (str) match {
      case Some (i) => create (i)
      case None =>
        Signal.unapply (str) match {
          case Some (s) => create (s)
          case None => create (Signal.Pass)
        }
    }
  }

  final case class Configuration (boardSize: Int, firstTurn: Player, handicap: Map[Intersection, Player], komi: Float)
  object Configuration {
    val default = Configuration (9, Player.Montague,  Map (), 5.5f)
  }

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

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Board (size: Int, private val grid: Matrix[Option[Colour]]) {
    assert (grid.columnCount == size)
    assert (grid.rowCount == size)

    def apply (s: String): Try[Option[Colour]] = Intersection.unapply (s) match {
      case Some (i) => apply (i)
      case None => Failure (Intersection.InvalidString)
    }
    def apply (i: Intersection): Try[Option[Colour]] =
      Try { grid.apply (i.x, i.y) }
        .recoverWith { case _ => Failure (Board.InvalidIntersectionException) }

    // Changes the state of the given intersection, without any regard for the rules!
    private def updated (i: Intersection, state: Option[Colour]): Try[Board] =
      Try { grid.updated (i.x, i.y, state) }
        .recoverWith { case _ => Failure (Board.InvalidIntersectionException) }
        .map (Board (size, _))

    private def cleared (i: Intersection): Try[Board] =
      updated (i, None)

    private def cleared (hs: HashSet[Intersection]): Try[Board] =
      hs.foldLeft (Try (this)) { (a, i) => a.flatMap (_.cleared (i)) }

    // This function doesn't care about who's move it actually it or the history
    // of the game.  The function simply takes a board, an intersection and a
    // colour, and tries to update the board according to the rules of Go; the
    // function fails if the move is illegal.  The fact the this fn doesn't know
    // who's move it is or the history of the game is important, because it means
    // the the function is unable to detect illegal moves due to Ko scenarios.
    def applyPlay (s: String, colour: Colour): Try[Board] = Intersection.unapply (s) match {
      case Some (i) => applyPlay (i, colour)
      case None => Failure (Intersection.InvalidString)
    }
    def applyPlay (i: Intersection, colour: Colour): Try[Board] = {
      apply (i) match {
        // make sure that the target intersection is empty
        case Success (None) =>
          // update the board
          updated (i, Some (colour)).flatMap { boardWithPlay =>
            val opposingGroups = boardWithPlay.groups (colour.opposition)
            val e = Map.empty[Group, HashSet[Intersection]]
            opposingGroups
                // map each opposing group to that group's liberties
                .map (group => (group, group.liberties.run (boardWithPlay)))
                // flatten out the Trys
                .foldLeft (Try (e)) { case (accT, (group, libertiesT)) => for {
                    accBoard <- accT
                    liberties <- libertiesT
                  } yield accBoard + (group -> liberties)
                }
                // collect any opposing groups that have no remaining liberties
                .map (_.toList.collect { case (g, l) if l.isEmpty => (g, l) }.toMap)
                // remove each opposing group with no remaining liberties from the board.
                .flatMap {
                  _.foldLeft (Try (boardWithPlay)) { case (accT, (group, liberties)) => for {
                      accBoard <- accT
                      newBoard <- accBoard.cleared (group.locations)
                    } yield newBoard
                  }
                }
          }
        case Success (Some (_)) => Failure[Board] (Board.IntersectionOccupiedException)
        case Failure (e) => Failure[Board] (e)
      }
    }

    def stoneLocations: HashSet[Intersection] = HashSet () ++ grid
      .zipWithAxes
      .collect { case (Some (value), x, y) => Intersection (x, y) }

    def stones: Map[Intersection, Colour] = grid
      .zipWithAxes
      .collect { case (Some(value), x, y) => (Intersection(x, y), value) }
      .toMap

    def groups (colour: Colour): HashSet[Group] =
      groups.filter (g => g.colour == colour)

    def groups: HashSet[Group] = {
      import scala.annotation.tailrec
      @tailrec def grow (colour: Colour, locations: HashSet[Intersection]): HashSet[Intersection] = {
        val locations2: HashSet[Intersection] = HashSet () ++ locations.flatMap { i =>
          (i.neighbours.run (this).get + i) //todo: refactor out `get` call
            .map (ix => (ix, apply (ix)))
            .collect { case (ix, Success (Some (c))) if c == colour => ix }
        }
        if (locations == locations2) locations2
        else grow (colour, locations2)
      }
      HashSet () ++ stones
        .map { case (i, c) =>
          val hs = HashSet () + i
          val hs2 = grow (c, hs)
          Group (c, hs2)
        }
    }
    def stringify: String = {
      final case class StringifySettings (stonePadding: Int, gridPadding: Int) {
        val stoneSize: Int = 1 + (2* stonePadding)
        val padding: Int = (stonePadding * 2) + gridPadding
        assert (gridPadding >= 1)
        assert (stoneSize % 2 != 0)
        assert (stoneSize <= padding)
        val gridSize = size + ((size - 1) * padding)
      }

      // Is the `board print` at the given location occupied by a stone?
      def isOccupiedAt (i: Int, j: Int): Reader[StringifySettings, Option[Game.Colour]] = Kleisli { ss: StringifySettings =>
        if ((j >= - ss.stonePadding && j <= ss.gridSize + ss.stonePadding)
         && (i >= - ss.stonePadding && i <= ss.gridSize + ss.stonePadding)) {
          val aj = j % (ss.padding + 1)
          val ai = i % (ss.padding + 1)
          val jjo =
            if (aj <= ss.stonePadding) Some (j - aj)
            else if (aj > ss.padding - ss.stonePadding) Some (j + aj)
            else None
          val iio =
            if (ai <= ss.stonePadding) Some (i - ai)
            else if (ai > ss.padding - ss.stonePadding) Some (i + ai)
            else None
          (jjo, iio) match {
            case (Some (jj), Some (ii)) =>
              val y = jj / (ss.padding + 1)
              val x = ii / (ss.padding + 1)
              val point = Game.Intersection (x, y)
              apply (point) match {
                case Success (Some (p)) =>
                  val aj =
                    if (j < 0) (j + ss.padding + 1) % (ss.padding + 1)
                    else j % (ss.padding + 1)
                  val ai =
                    if (i < 0) (i + ss.padding + 1) % (ss.padding + 1)
                    else i % (ss.padding + 1)
                  val iEdge = ai == ss.stonePadding || ai == ss.padding - ss.stonePadding + 1
                  val jEdge = aj == ss.stonePadding || aj == ss.padding - ss.stonePadding + 1
                  if (jEdge && iEdge) None else Some (p)
                case _ => None
              }
            case _ => None
          }
        } else None
      }

      // What character represents an empty board at the given `board print` location.
      // (Draws a nice unicode grid based on the given padding settings).
      def emptyBoardAt (i: Int, j: Int): Reader[StringifySettings, Char] = Kleisli { ss: StringifySettings =>
        if (j == -ss.padding || j == ss.gridSize + ss.padding - 1 || i == -ss.padding || i == ss.gridSize + ss.padding - 1) '▒'
        else if (j < 0 || j > ss.gridSize - 1 || i < 0 || i > ss.gridSize - 1) ' '
        else {
          if (j == 0 && i == 0) '┏'
          else if (j == 0 && i == ss.gridSize - 1) '┓'
          else if (j == ss.gridSize - 1 && i == 0) '┗'
          else if (j == ss.gridSize - 1 && i == ss.gridSize - 1) '┛'
          else if (j == 0 && i % (ss.padding + 1) == 0) '┳'
          else if (j == ss.gridSize - 1 && i % (ss.padding + 1) == 0) '┻'
          else if (j % (ss.padding + 1) == 0 && i == 0) '┣'
          else if (j % (ss.padding + 1) == 0 && i == ss.gridSize - 1) '┫'
          else if (j % (ss.padding + 1) == 0 && i % (ss.padding + 1) == 0) '╋'
          else if (j % (ss.padding + 1) == 0) '━'
          else if (i % (ss.padding + 1) == 0) '┃'
          else ' '
        }
      }

      val ss = StringifySettings (2, 1)
      (-ss.padding until ss.gridSize + ss.padding).foldLeft (List.empty[String]) { (accj, j) =>
        val line = (-ss.padding until ss.gridSize + ss.padding).foldLeft ("") { (acci, i) =>
          val occupant = isOccupiedAt (i, j).run (ss)
          val charAtLocation = occupant match {
            case Some (o) if o == Game.Colour.Black => '▓'
            case Some (o) if o == Game.Colour.White => '░'
            case None => emptyBoardAt (i, j).run (ss)
          }
          acci + charAtLocation
        }
        accj :+ line
      }
    }.mkString (newLine)
  }
  object Board {

    object InvalidIntersectionException extends Exception
    object IntersectionOccupiedException extends Exception

    def createS (size: Int, data: Map [String, Colour]): Board = {
      val d = data
        .map { case (k, v) => (Intersection.unapply (k), v) }
        .collect { case (Some (k), v) => (k, v) }
        .toMap
      create (size, d)
    }
    def create (size: Int, data: Map [Intersection, Colour]): Board = {
      val g = Matrix.tabulate[Option[Colour]] (size, size) { (x, y) => Intersection (x, y) |> data.get }
      Board (size, g)
    }

    def create (size: Int): Board = {
      val g = Matrix.tabulate[Option[Colour]] (size, size) { (_, _) => None }
      Board (size, g)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Intersection (x: Int, y: Int) { // zero indexed
    override def toString = if (x < 26) ('A'.toInt + x).toChar + (y + 1).toString else toString
    def + (i: Intersection) = Intersection (x + i.x, y + i.y)
    def - (i: Intersection) = Intersection (x - i.x, y - i.y)
    def north = this + Intersection (0, 1)
    def east = this + Intersection (1, 0)
    def south = this + Intersection (0, -1)
    def west = this + Intersection (-1, 0)

    // Given a board, if this intersection exists on that board, returns the set of neighbouring connected `valid`
    // intersections.
    def neighbours: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
      board (this).map { _ => HashSet () ++ (north :: east :: south :: west :: Nil)
        .map { i => (i, board (i)) }
        .collect { case (i, Success (x)) => i }
      }
    }
  }

  object Intersection {
    object InvalidString extends Exception
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


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Group (colour: Colour, locations: HashSet[Intersection]) {
    // Does this group exist on the given board?
    def isValid: Reader[Board, Boolean] = Reader { board: Board => board.groups.contains (this) }

    // Given a board, if this group exists on that board, returns the set of
    // neighbouring connected `valid` intersections.
    def neighbours: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
      isValid
        .run (board) match {
        case false => Failure[HashSet[Intersection]] (Group.GroupInvalidForBoardException)
        case true => locations
          .map (_.neighbours.run (board))
          .reduce { (a, b) => for {aa <- a; bb <- b} yield aa ++ bb }
      }
    }

    // Given a board, if this group exists on that board, returns the set of
    // neighbouring connected `valid` intersections.
    def liberties: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
      neighbours
        .run (board)
        .map { n => HashSet () ++ n
          .map { i => (i, board (i)) }
          .collect { case (i, Success (None)) => i }
        }
    }

    // Given a board, if this group exists on that board, returns the set of
    // neighbouring connected intersections occupied by the opposing colour.
    def connections: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
      neighbours
        .run (board)
        .map { n => HashSet () ++ n
          .map { i => (i, board (i)) }
          .collect { case (i, Success (Some (c))) if c == colour.opposition => i }
        }
    }
  }
  object Group {
    object GroupInvalidForBoardException extends Exception
  }
}
