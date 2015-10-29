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
  def findAll (f: T => Boolean): List [(Int, Int)] = {
    //println ("Matrix.findAll")
    zipWithAxes.collect {
      case (value, x, y) if f (value) => (x, y)
    }
  }

  def axes: HashSet[(Int, Int)] = {
    val result = HashSet() ++ (0 until columnCount).flatMap (x => (0 until rowCount).map (y => (x, y)))
    //println (s"Matrix.axes = ${result.size}")
    result
  }

  def zipWithAxes: List[(T, Int, Int)] = {
    val result = axes.map { case (x, y) =>
      //println (s"Matrix.zipWithAxes map (x:$x, y:$y)")
      (apply (x, y), x, y)
    }.toList
    //println (s"Matrix.zipWithAxes = ${result.size}")
    result
  }
}
object Matrix {
  def tabulate[T] (rowCount: Int, columnCount: Int)(f: (Int, Int) => T): Matrix[T] = {
    //println (s"Matrix.tabulate (rowCount:$rowCount, columnCount:$columnCount)")
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
    //println (s"Matrix.apply (row:$row, column:$column)")
    if (row < 0 || row >= rowCount || column < 0 || column >= columnCount)
      throw new IndexOutOfBoundsException
    else {
      val result = data (row + column * rowCount)
      //println (s"Matrix.apply = $result")
      result
    }
  }
  def updated (row: Int, column: Int, value: T): Matrix[T] = {
   // println (s"Matrix.updated (row:$row, column:$column, value:$value)")
    if (row < 0 || row >= rowCount || column < 0 || column >= columnCount)
      throw new IndexOutOfBoundsException
    else MatrixImpl (rowCount, columnCount, data.updated (row + column * rowCount, value))
  }
}
