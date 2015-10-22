package hayago

object Game {
  import scala.util._
  import com.github.nscala_time.time.Imports._
  import scala.collection.immutable.HashSet

  object Logic {
    case class Group (stones: HashSet[Board.Intersection]){
      def liberties: HashSet[Board.Intersection] = ???
    }

    // Aggregate groups of stones on the board for a particular colour.
    def aggregate (board: Board, colour: Colour): HashSet[Group] = ???

    // Applies a play to the board.
    // this fn doesn't care about who's move it actually it.
    // it just takes a board, a point and a colour, and updates the board state according to the rules of go.
    def play (board: Board, i: Board.Intersection, colour: Colour): Board = {
      // if any of the opposing group's have a single remaining liberty at the location of this play,
      // then collect all stones in that group for removal.
      val oppositionToRemove = aggregate (board, Colour.opposing (firstTurnColour)).toList.collect { case group if
        group.liberties.contains (i) && group.liberties.size <= 1 => group.stones.toList
      }.flatten
      val boardWithPlay = board
        .clear (oppositionToRemove)
        .add (i, colour) // now add the play to the board
      // Perhaps you made a suicide move.  Not sure if doing so is actually illegal.  If it is then this
      // bit can be removed.
      val friendlyGroups =  aggregate (boardWithPlay, colour)
      val friendsToRemove = friendlyGroups.toList.collect { case group if
        group.liberties.isEmpty => group.stones.toList
      }.flatten
      board.clear (friendsToRemove)
    }
  }

  final case class Board (private val grid: Matrix[Option[Colour]]) {
    val size: Int = grid.rowCount
    // get
    def apply (i: Board.Intersection): Option[Colour] = grid (i.x, i.y).getOrElse (None)
    def get (i: Board.Intersection): Option[Colour] = apply (i)

    def add (i: Board.Intersection, colour: Colour): Board = update (i, Some (colour))

    // update
    def update (i: Board.Intersection, state: Option[Colour]): Board = grid.update (i.x, i.y, state).map (Board (_)).getOrElse (this)

    // clear
    def clear (i: Board.Intersection): Board = update (i, None)
    def clear (hs: List[Board.Intersection]): Board = hs.foldLeft (this) { (a, i) => a.clear (i) }
  }
  object Board {
    // counting from zero
    case class Intersection (x: Int, y: Int) {
      override def toString = if (x < 26) ('A'.toInt + x).toChar + (y + 1).toString else toString
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
    def play (i: Board.Intersection) = Turn (Right (i))
  }

  sealed trait Player
  object Player {
    def opposing (player: Player) = player match {
      case Montague => Capulet
      case Capulet => Montague
    }
  }
  object Montague extends Player // Man
  object Capulet extends Player  // Computer

  sealed trait Colour
  object Colour {
    def opposing (colour: Colour) = colour match {
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
      val numIntersections = setup.boardSize * setup.boardSize
      val empty = Board (Matrix[Option[Colour]] (numIntersections, numIntersections))
      val withHandicap = setup.handicap.toList.foldLeft (empty) { (a, i) =>
        val intersection = i._1
        val player = i._2
        Logic.play (a, intersection, colour (player))
      }
      history.indices.foldLeft (withHandicap) { (a, i) =>
        val turn = history (i)
        val colour = colourToPlayTurn (i)
        turn.action match {
          case Left (_) => a
          case Right (intersection) => Logic.play (a, intersection, colour)
        }
      }
    }


    def playerToPlayTurn (index: Int): Player = if (index % 2 == 0) setup.firstTurn else Player.opposing (setup.firstTurn)
    def playerToPlayNext = playerToPlayTurn (history.size)

    def colourToPlayTurn (index: Int): Colour = if (playerToPlayTurn (index) == setup.firstTurn) firstTurnColour else Colour.opposing (firstTurnColour)
    def colourToPlayNext = colourToPlayTurn (history.size)

    def colour (player: Player) = if (player == setup.firstTurn) firstTurnColour else Colour.opposing (firstTurnColour)
    def player (colour: Colour) = if (colour == Black) setup.firstTurn else Player.opposing (setup.firstTurn)

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