package hayago.game

import scala.util._
import scala.collection.immutable.HashSet
import cats.syntax.eq._
import cats.data.{ReaderT, Reader, Kleisli}

final case class Group (colour: Colour, locations: HashSet[Intersection]) {
  // Does this group exist on the given board?
  lazy val exists: Reader[Board, Boolean] = Reader { board: Board => board.groups.contains (this) }

  // Given a board, if this group exists on that board, returns the set of
  // neighbouring connected `valid` intersections.
  lazy val neighbours: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
    exists
      .run (board) match {
      case false => Failure[HashSet[Intersection]] (Group.GroupInvalidForBoardException)
      case true => locations
        .map (_.neighbours.run (board))
        .reduce { (a, b) => for { aa <- a; bb <- b } yield aa ++ bb }
    }
  }

  // Given a board, if this group exists on that board, returns the set of
  // neighbouring connected `valid` intersections.
  lazy val liberties: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
    neighbours
      .run (board)
      .map { n => HashSet () ++ n
        .map { i => (i, board (i)) }
        .collect { case (i, Success (None)) => i }
      }
  }

  // Given a board, if this group exists on that board, returns the set of
  // neighbouring connected intersections occupied by the opposing colour.
  lazy val connections: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
    neighbours
      .run (board)
      .map { n => HashSet () ++ n
        .map { i => (i, board (i)) }
        .collect { case (i, Success (Some (c))) if c === colour.opposition => i }
      }
  }
}
object Group {
  object GroupInvalidForBoardException extends Exception
}
