package hayago.game

import hayago._
import scala.util._
import scala.collection.immutable.HashSet
import cats.data.{ReaderT, Reader, Kleisli}

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