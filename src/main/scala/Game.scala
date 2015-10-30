package hayago

object Game {
  import scala.util._
  import com.github.nscala_time.time.Imports._
  import scala.collection.immutable.HashSet
  import cats.data.{ReaderT, Reader, Kleisli}

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val firstTurnColour = Colour.Black // Black is always first

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Signal
  object Signal {
    object Pass extends Signal { override def toString = "PASS" }
    object Resign extends Signal { override def toString = "RESIGN" }
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Player
  object Player {
    def opposition (player: Player) = player match {
      case Montague => Capulet
      case Capulet => Montague
    }
    object Montague extends Player { override def toString = "MONTAGUE" } // Man
    object Capulet extends Player { override def toString = "CAPULET" } // Computer
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

  case class Turn (action: Either[Signal, Board.Intersection], time: DateTime)
  object Turn {
    def create (s: String): Turn = Board.Intersection.unapply (s).map (create).getOrElse (Turn (Left (Signal.Pass), DateTime.now))
    def create (i: Board.Intersection): Turn = Turn (Right (i), DateTime.now)
    def create (s: Signal) = Turn (Left (s), DateTime.now)
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class Configuration (boardSize: Int, firstTurn: Player, handicap: Map[Board.Intersection, Player], komi: Float)
  object Configuration {
    val default = Configuration (19, Player.Montague,  Map (), 6.5f)
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class State (setup: Configuration, history: List[Turn] = Nil) {

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

    def playerToPlayTurn (index: Int): Player = if (index % 2 == 0) setup.firstTurn else Player.opposition (setup.firstTurn)

    def playerToPlayNext = playerToPlayTurn (history.size)

    def colourToPlayTurn (index: Int): Colour = if (playerToPlayTurn (index) == setup.firstTurn) firstTurnColour else firstTurnColour.opposition

    def colourToPlayNext = colourToPlayTurn (history.size)

    def colour (player: Player) = if (player == setup.firstTurn) firstTurnColour else firstTurnColour.opposition

    def player (colour: Colour) = if (colour == Colour.Black) setup.firstTurn else Player.opposition (setup.firstTurn)

    def isTurnLegal (turn: Turn): Boolean = {
      turn.action match {
        case Right (i) => board.applyPlay (i, colourToPlayNext) match {
          case Success (boardWithPlay) =>
            !HashSet (allBoards: _*).contains (boardWithPlay)
          case Failure (_) => false
        }
        case Left (_) => true
      }
    }

    def isComplete: Boolean = history.collect { case t @ Turn (Left (Signal.Resign), _) => () }.isEmpty match {
      case false => false
      case true => history.reverse match {
        case first :: second :: _ => (first, second) match {
          case (Turn (Left (Signal.Pass), _), Turn (Left (Signal.Pass), _)) => true
          case _ => false
        }
        case _ => false
      }
    }
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Board (size: Int, private val grid: Matrix[Option[Colour]]) {

    object InvalidIntersectionException extends Exception
    object IntersectionOccupiedException extends Exception
    object InvalidConstructorException extends Exception

    if (grid.columnCount != size || grid.rowCount != size) throw InvalidConstructorException

    /////////////////////////////////////////////////////////////////////////////

    def apply (i: Board.Intersection): Try[Option[Colour]] =
      Try { grid.apply (i.x, i.y) }
        .recoverWith { case _ => Failure (InvalidIntersectionException) }

    def apply (s: String): Try[Option[Colour]] =
      Game.Board.Intersection.unapply (s) match {
        case Some (i) => apply (i)
        case None => Failure (Board.Intersection.InvalidString)
      }

    /////////////////////////////////////////////////////////////////////////////

    /**
     * Changes the state of the given intersection, without any regard for the rules!
     */
    private def updated (i: Board.Intersection, state: Option[Colour]): Try[Board] =
      Try { grid.updated (i.x, i.y, state) }
        .recoverWith { case _ => Failure (InvalidIntersectionException) }
        .map (Board (size, _))

    private def cleared (i: Board.Intersection): Try[Board] = updated (i, None)

    private def cleared (hs: HashSet[Board.Intersection]): Try[Board] = hs.foldLeft (Try (this)) { (a, i) => a.flatMap (_.cleared (i)) }

    /////////////////////////////////////////////////////////////////////////////

    // This function doesn't care about who's move it actually it or the history
    // of the game.  The function simply takes a board, an intersection and a
    // colour, and tries to update the board according to the rules of Go; the
    // function fails if the move is illegal.  The fact the this fn doesn't know
    // who's move it is or the history of the game is important, because it means
    // the the function is unable to detect illegal moves due to Ko scenarios.
    def applyPlay (i: Board.Intersection, colour: Colour): Try[Board] = {
      apply (i) match {
        // make sure that the target intersection is empty
        case Success (None) =>
          // update the board
          updated (i, Some (colour)).flatMap { boardWithPlay =>
            val opposingGroups = boardWithPlay.groups (colour.opposition)
            val e = Map.empty[Board.Group, HashSet[Board.Intersection]]
            opposingGroups
                // map each opposing group to that group's liberties
                .map (group => (group, group.liberties.run (boardWithPlay)))
                // flatten out the Trys
                .foldLeft (Try (e)) { case (accT, (group, libertiesT)) => for { accBoard <- accT; liberties <- libertiesT } yield accBoard + (group -> liberties) }
                // collect any opposing groups that have no remaining liberties
                .map (_.toList.collect { case (g, l) if l.isEmpty => (g, l) }.toMap)
                // remove each opposing group with no remaining liberties from the board.
                .flatMap (_.foldLeft (Try (boardWithPlay)) { case (accT, (group, liberties)) => for { accBoard <- accT; newBoard <- accBoard.cleared (group.locations) } yield newBoard })
          }
        case Success (Some (_)) => Failure[Board] (IntersectionOccupiedException)
        case Failure (e) => Failure[Board] (e)
      }
    }

    def stoneLocations: HashSet[Board.Intersection] = HashSet () ++ grid
      .zipWithAxes
      .collect { case (Some (value), x, y) => Board.Intersection (x, y) }

    def stones: Map[Board.Intersection, Colour] = grid
      .zipWithAxes
      .collect { case (Some(value), x, y) => (Board.Intersection(x, y), value) }
      .toMap

    def groups (colour: Colour): HashSet[Board.Group] = groups.filter (g => g.colour == colour)

    def groups: HashSet[Board.Group] = {
      import scala.annotation.tailrec
      @tailrec def grow (colour: Colour, locations: HashSet[Board.Intersection]): HashSet[Board.Intersection] = {
        val locations2: HashSet[Board.Intersection] = HashSet () ++ locations.flatMap { i =>
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
          Board.Group (c, hs2)
        }
    }
  }
  object Board {

    def create (size: Int, data: Map [Board.Intersection, Colour]): Board = {
      val g = Matrix.tabulate[Option[Colour]] (size, size) { (x, y) => data.get (Board.Intersection (x, y)) }
      Board (size, g)
    }

    def create (size: Int): Board = {
      val g = Matrix.tabulate[Option[Colour]] (size, size) { (_, _) => None }
      Board (size, g)
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    final case class Group (colour: Colour, locations: HashSet[Board.Intersection]) {

      object GroupInvalidForBoardException extends Exception

      def isValid: Reader[Board, Boolean] = Reader { board: Board => board.groups.contains (this) }

      // Given a board, if this group exists on that board, returns the set of
      // neighbouring connected `valid` intersections.
      def neighbours: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        isValid
          .run (board) match {
          case false => Failure[HashSet[Board.Intersection]] (GroupInvalidForBoardException)
          case true => locations
            .map (_.neighbours.run (board))
            .reduce { (a, b) => for {aa <- a; bb <- b} yield aa ++ bb }
        }
      }

      // Given a board, if this group exists on that board, returns the set of
      // neighbouring connected `valid` intersections.
      def liberties: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        neighbours
          .run (board)
          .map { n => HashSet () ++ n
            .map { i => (i, board (i)) }
            .collect { case (i, Success (None)) => i }
          }
      }

      // Given a board, if this group exists on that board, returns the set of
      // neighbouring connected intersections occupied by the opposing colour.
      def connections: ReaderT[Try, Board, HashSet[Board.Intersection]] = Kleisli { board: Board =>
        neighbours
          .run (board)
          .map { n => HashSet () ++ n
            .map { i => (i, board (i)) }
            .collect { case (i, Success (Some (c))) if c == colour.opposition => i }
          }
      }
    }
  }
}
