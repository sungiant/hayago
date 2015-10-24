package hayago

import scala.util.Try

case class MatrixImpl[T] (private val data: List[T]) extends Matrix[T] {
  def rowCount: Int = ???
  def columnCount: Int = ???
  def apply (row: Int, column: Int): Try[T] = ???
  def update (row: Int, column: Int, value: T): Try[Matrix[T]] = ???
}

abstract class Matrix[T] {
  def rowCount: Int
  def columnCount: Int
  def apply (row: Int, column: Int): Try[T]
  def update (row: Int, column: Int, value: T): Try[Matrix[T]]
}
object Matrix {
  def apply[T] (rowCount: Int, columnCount: Int): Matrix[T] = ???
}
