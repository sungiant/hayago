package hayago.game

import hayago._
import scala.util._
import scala.collection.immutable.HashSet
import cats.std.all._
import cats.syntax.eq._
import cats.data.{ReaderT, Reader, Kleisli}

final case class Territory (locations: HashSet[Intersection]) {

  override lazy val toString = "Territory (" + locations.map (_.toString).mkString (", ") + ")"

  lazy val stringify: ReaderT[Try, Board, String] = Kleisli { board: Board =>
    controlledBy.run (board).map { x =>
      toString + ", controlled by:" + x
    }
  }

  // Does this group exist on the given board?
  lazy val exists: Reader[Board, Boolean] = Reader { board: Board => board.territories.contains (this) }

  // Given a board, if this group exists on that board, returns the set of
  // neighbouring connected `valid` intersections.
  lazy val neighbours: ReaderT[Try, Board, HashSet[Intersection]] = Kleisli { board: Board =>
    exists
      .run (board) match {
      case false => Failure[HashSet[Intersection]] (Territory.TerritoryInvalidForBoardException)
      case true => locations
        .map (_.neighbours.run (board))
        .reduce { (a, b) => for { aa <- a; bb <- b } yield aa ++ bb }
    }
  }

  // Given a board, if this territory exists on that board, does the territory have a controller?
  lazy val controlledBy: ReaderT[Try, Board, Option[Colour]] = Kleisli { board: Board =>
    neighbours
      .run (board)
      .map { n =>
        val d = n
          .map { i => (i, board(i)) }
          .collect { case (i, Success(o)) => o }
          .toList
        val dx = d.distinct
        dx.length match {
          case 2 => dx.contains (None) match {
            case true => dx.filterNot (x => x === None).head
            case false => None
          }
          case 1 => d.head
          case _ => None
        }
      }
  }
}
object Territory {
  object TerritoryInvalidForBoardException extends Exception
}
