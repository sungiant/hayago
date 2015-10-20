package hayago

import scala.util.Try

abstract class Grid[T] {
  val size: Int
  def get (point: Point): Try[T]
  def update (point: Point, value: T): Try[Grid[T]]
}
object Grid {
  def apply[T] (size: Int): Grid[T] = ???
}
