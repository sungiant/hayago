package hayago

import scala.util.Try

case class MatrixImpl[T] (private val data: List[T]) extends Matrix[T] {
  val rowCount: Int = ???
  val columnCount: Int = ???
  def apply (row: Int, column: Int): Try[T] = ???
  def update (row: Int, column: Int, value: T): Try[Matrix[T]] = ???
}

abstract class Matrix[T] {
  val rowCount: Int
  val columnCount: Int
  def apply (row: Int, column: Int): Try[T]
  def update (row: Int, column: Int, value: T): Try[Matrix[T]]
}
object Matrix {
  def apply[T] (rowCount: Int, columnCount: Int): Matrix[T] = ???
}
