package hayago.game

import hayago._
import scala.util._
import scala.collection.immutable.HashSet
import cats.data.{ReaderT, Reader, Kleisli}

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
        .reduce { (a, b) => for { aa <- a; bb <- b } yield aa ++ bb }
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