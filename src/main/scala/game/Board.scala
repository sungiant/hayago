package hayago.game

import hayago._
import scala.util._
import scala.collection.immutable.HashSet
import cats.std.all._
import cats.syntax.eq._
import cats.data.{Reader, Kleisli}
import scala.annotation.tailrec

/**
 * The Board case class can be used to represent a snapshot of the state of the board, the class knows:
 * - about the rules of Go
 * - where every stone is on the board
 * - the size of the board
 * - nothing about the game itself (who has the next turn, how many turns have there been, etc...)
 */
final case class Board (size: Int, private val grid: Matrix[Option[Colour]]) {
  assert (grid.columnCount === size)
  assert (grid.rowCount === size)

  def apply (s: String): Try[Option[Colour]] = Intersection.unapply (s) match {
    case Some (i) => apply (i)
    case None => Failure (Board.InvalidIntersectionException)
  }
  def apply (i: Intersection): Try[Option[Colour]] =
    Try { grid.apply (i.x, i.y) }
      .recoverWith { case _ => Failure (Board.InvalidIntersectionException) }

  /**
   * Changes the state of the given intersection, without any regard for the rules!
   */
  private def updated (i: Intersection, state: Option[Colour]): Try[Board] =
    Try { grid.updated (i.x, i.y, state) }
      .recoverWith { case _ => Failure (Board.InvalidIntersectionException) }
      .map (Board (size, _))

  private def cleared (i: Intersection): Try[Board] =
    updated (i, None)

  def without (hs: HashSet[Intersection]): Try[Board] =
    hs.foldLeft (Try (this)) { (a, i) => a.flatMap (_.cleared (i)) }

  /**
   * This function doesn't care about who's move it actually it or the history
   * of the   The function simply takes a board, an intersection and a
   * colour, and tries to update the board according to the rules of Go; the
   * function fails if the move is illegal.  The fact the this fn doesn't know
   * who's move it is or the history of the game is important, because it means
   * the the function is unable to detect illegal moves due to Ko scenarios.
   */
  def applyMove (s: String, colour: Colour): Try[Board] = Intersection.unapply (s) match {
    case Some (i) => applyMove (i, colour)
    case None => Failure (Board.InvalidIntersectionException)
  }
  def applyMove (i: Intersection, colour: Colour): Try[Board] = {
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
                    newBoard <- accBoard.without (group.locations)
                  } yield newBoard
                }
              }
        }
      case Success (Some (_)) => Failure[Board] (Board.IntersectionOccupiedException)
      case Failure (e) => Failure[Board] (e)
    }
  }

  def legalNextMovesFor (colour: Colour): HashSet[Intersection] = {
    unoccupiedLocations.map { i =>
      applyMove (i, colour) match {
        case Success (_) => Some (i)
        case Failure (_) => None
      }
    }.collect { case Some (t) => t }
  }

  lazy val stoneLocations: HashSet[Intersection] = HashSet () ++ grid
    .zipWithAxes
    .collect { case (Some (value), x, y) => Intersection (x, y) }

  lazy val stones: Map[Intersection, Colour] = grid
    .zipWithAxes
    .collect { case (Some(value), x, y) => (Intersection(x, y), value) }
    .toMap

  def stoneCount (colour: Colour) = stones.collect { case (k, v) if v === colour => () }.size

  lazy val unoccupiedLocations: HashSet[Intersection] = HashSet () ++ grid
    .zipWithAxes
    .collect { case (None, x, y) => Intersection (x, y) }

  def groups (colour: Colour): HashSet[Group] =
    groups.filter (g => g.colour === colour)

  lazy val groups: HashSet[Group] = {
    @tailrec def grow (colour: Colour, locations: HashSet[Intersection]): HashSet[Intersection] = {
      locations.flatMap { i =>
        (i.neighbours.run (this).get + i) //todo: refactor out `get` call
          .map (ix => (ix, apply (ix)))
          .collect { case (ix, Success (Some (c))) if c === colour => ix }
      } match {
        case updatedLocations if updatedLocations == locations => updatedLocations
        case updatedLocations => grow (colour, updatedLocations)
      }
    }
    stones.foldLeft (HashSet[Group] ()) { case (acc, (i, c)) =>
      acc.exists (g => g.locations.contains (i)) match {
        case true => acc
        case false => acc + Group (c, grow (c, HashSet(i)))
      }
    }
  }

  /**
   * Works out territory groups for this board.
   * This function has no concept of agreements surrounding dead stones,
   * so this function simply treats all stones on the board
   * as if they are alive.
   */
  lazy val territories: HashSet[Territory] = {
    @tailrec def grow (locations: HashSet[Intersection]): HashSet[Intersection] = {
      locations.flatMap { i =>
        (i.neighbours.run (this).get + i) //todo: refactor out `get` call
          .map (ix => (ix, apply (ix)))
          .collect { case (ix, Success (None)) => ix }
      } match {
        case updatedLocations if updatedLocations == locations => updatedLocations
        case updatedLocations => grow (updatedLocations)
      }
    }
    unoccupiedLocations.foldLeft (HashSet[Territory] ()) { (acc, i) =>
      acc.exists (g => g.locations.contains (i)) match {
        case true => acc
        case false => acc + Territory (grow (HashSet (i)))
      }
    }
  }

  /**
   * Generates a human readable ASCII string representation of the board.
   */
  lazy val stringify: String = {
    final case class StringifySettings (stonePadding: Int, gridPadding: Int) {
      val stoneSize: Int = 1 + (2* stonePadding)
      val padding: Int = (stonePadding * 2) + gridPadding
      assert (gridPadding >= 1)
      assert (stoneSize % 2 =!= 0)
      assert (stoneSize <= padding)
      val gridSize = size + ((size - 1) * padding)
    }

    // Is the `board print` at the given location occupied by a stone?
    def isOccupiedAt (i: Int, j: Int): Reader[StringifySettings, Option[Colour]] = Kleisli { ss: StringifySettings =>
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
            val point = Intersection (x, y)
            apply (point) match {
              case Success (Some (p)) =>
                val aj =
                  if (j < 0) (j + ss.padding + 1) % (ss.padding + 1)
                  else j % (ss.padding + 1)
                val ai =
                  if (i < 0) (i + ss.padding + 1) % (ss.padding + 1)
                  else i % (ss.padding + 1)
                val iEdge = ai === ss.stonePadding || ai === ss.padding - ss.stonePadding + 1
                val jEdge = aj === ss.stonePadding || aj === ss.padding - ss.stonePadding + 1
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
      if (j === -ss.padding || j === ss.gridSize + ss.padding - 1 || i === -ss.padding || i === ss.gridSize + ss.padding - 1) '▒'
      else if (j < 0 || j > ss.gridSize - 1 || i < 0 || i > ss.gridSize - 1) ' '
      else {
        if (j === 0 && i === 0) '┏'
        else if (j === 0 && i === ss.gridSize - 1) '┓'
        else if (j === ss.gridSize - 1 && i === 0) '┗'
        else if (j === ss.gridSize - 1 && i === ss.gridSize - 1) '┛'
        else if (j === 0 && i % (ss.padding + 1) === 0) '┳'
        else if (j === ss.gridSize - 1 && i % (ss.padding + 1) === 0) '┻'
        else if (j % (ss.padding + 1) === 0 && i === 0) '┣'
        else if (j % (ss.padding + 1) === 0 && i === ss.gridSize - 1) '┫'
        else if (j % (ss.padding + 1) === 0 && i % (ss.padding + 1) === 0) '╋'
        else if (j % (ss.padding + 1) === 0) '━'
        else if (i % (ss.padding + 1) === 0) '┃'
        else ' '
      }
    }

    val ss = StringifySettings (2, 1)
    (-ss.padding until ss.gridSize + ss.padding).foldLeft (List.empty[String]) { (accj, j) =>
      val line = (-ss.padding until ss.gridSize + ss.padding).foldLeft ("") { (acci, i) =>
        val occupant = isOccupiedAt (i, j).run (ss)
        val charAtLocation = occupant match {
          case Some (o) if o === Colour.Black => '▓'
          case Some (o) if o === Colour.White => '░'
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

  // Not sure where this fn should live...
  def identifyDeadStones (board: Board): HashSet[Intersection] = HashSet ()

  def createS (size: Int, data: Map [String, Colour]): Board = {
    val d = data
      .map { case (k, v) => (Intersection.unapply (k), v) }
      .collect { case (Some (k), v) => (k, v) }
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
