package hayago

import scala.collection.Iterator
import scala.util.Try
import scala.collection.immutable.HashSet

/*
 * An immutable matrix type.
 */
abstract class Matrix[T] {
  def rowCount: Int
  def columnCount: Int
  def apply (row: Int, column: Int): T
  def updated (row: Int, column: Int, value: T): Matrix[T]

  // find co-ordinates of all elements that match a predicate
  def findAll (f: T => Boolean): List [(Int, Int)] = zipWithAxes.collect {
    case (value, x, y) if f (value) => (x, y)
  }

  def axes: HashSet[(Int, Int)] = HashSet() ++ (0 until columnCount).flatMap (x => (0 until rowCount).map (y => (x, y)))

  def zipWithAxes: List[(T, Int, Int)] = axes.map { case (x, y) =>
    (apply (x, y), x, y)
  }.toList
}
object Matrix {
  object AxesOutOfBoundsException extends Exception
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
      throw Matrix.AxesOutOfBoundsException
    else data (row + column * rowCount)
  }
  def updated (row: Int, column: Int, value: T): Matrix[T] = {
    if (row < 0 || row >= rowCount || column < 0 || column >= columnCount)
      throw Matrix.AxesOutOfBoundsException
    else MatrixImpl (rowCount, columnCount, data.updated (row + column * rowCount, value))
  }
}
