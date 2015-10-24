package hayago

import scala.util.Try

/*
 * An immutable matrix type.
 */
abstract class Matrix[T] {
  val rowCount: Int
  val columnCount: Int
  def apply (row: Int, column: Int): T
  def updated (row: Int, column: Int, value: T): Matrix[T]
}
object Matrix {
  def tabulate[T] (rowCount: Int, columnCount: Int)(f: (Int, Int) => T): Matrix[T] = {
    if (rowCount <= 0 || columnCount <= 0)
      throw new NegativeArraySizeException
    else {
      val data = (0 until rowCount * columnCount).map { index =>
        val x = index % rowCount
        val y = (index - x) / columnCount
        f (x, y)
      }.toList
      MatrixImpl (rowCount, columnCount, data)
    }
  }
}


case class MatrixImpl[T] (rowCount: Int, columnCount: Int, private val data: List[T]) extends Matrix[T] {
  def apply (row: Int, column: Int): T = {
    if (row < 0 || row >= rowCount || column < 0 || column >= columnCount)
      throw new IndexOutOfBoundsException
    else data (row + column * rowCount)
  }
  def updated (row: Int, column: Int, value: T): Matrix[T] = {
    if (row < 0 || row >= rowCount || column < 0 || column >= columnCount)
      throw new IndexOutOfBoundsException
    else MatrixImpl (rowCount, columnCount, data.updated (row + column * rowCount, value))

  }
}