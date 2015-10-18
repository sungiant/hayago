package hayago

import scala.util._
import scala.text._
import cats.std.all._
import cats.syntax.all._

object PrettyPrinter {
  private val newline = System.getProperty("line.separator")

  // yuck ~ copied from my old C# implementation.
  // todo: implement pure functional version.
  def print (boardState: Array[Array[Option[Engine.Colour]]]): Unit = println (stringify (boardState))
  def stringify (boardState: Array[Array[Option[Engine.Colour]]]): String = {
    val boardSize = boardState.length
    val stonePadding = 2
    val stoneSize = 1 + (2* stonePadding)
    val gridPadding = 1
    val padding = (stonePadding * 2) + gridPadding

    assert (gridPadding >= 1)
    assert (stoneSize % 2 != 0)
    assert (stoneSize <= padding)

    val sb = new StringBuilder()
    val gridSize = boardSize + ((boardSize - 1) * padding)

    for (j <- -padding until gridSize + padding) {
      for (i <- -padding until gridSize + padding) {
        var p: Option[Engine.Colour] = None

        if ((j >= - stonePadding && j <= gridSize + stonePadding) && (i >= - stonePadding && i <= gridSize + stonePadding)) {
          val aj = j % (padding + 1)
          val ai = i % (padding + 1)
          var jj: Option[Int] = None
          var ii: Option[Int] = None

          if (aj <= stonePadding) {
            jj = Some (j - aj)
          }
          else if (aj > padding - stonePadding) {
            jj = Some (j + aj)
          }

          if (ai <= stonePadding) {
            ii = Some (i - ai)
          }
          else if (ai > padding - stonePadding) {
            ii = Some (i + ai)
          }

          if (jj.isDefined && ii.isDefined) {
            val y = jj.get / (padding + 1)
            val x = ii.get / (padding + 1)
            p = boardState (x)(y)
          }
        }

        if (p.isDefined) {
          var iEdge = false
          var jEdge = false

          var aj = j % (padding + 1)
          if (j < 0) {
            aj = (j + padding + 1) % (padding + 1)
          }

          var ai = i % (padding + 1)
          if (i < 0) {
            ai = (i + padding + 1) % (padding + 1)
          }

          if (aj == stonePadding || aj == padding - stonePadding + 1) {
            jEdge = true
          }

          if (ai == stonePadding || ai == padding - stonePadding + 1) {
            iEdge = true
          }

          if (jEdge && iEdge) {
            p = None
          }
        }

        if (p.isDefined && p.get == Engine.Black) sb.append("?")
        else if (p.isDefined && p.get == Engine.White) sb.append("?")
        else {
          if (j < 0 || j > gridSize - 1 || i < 0 || i > gridSize - 1) sb.append(" ")
          else {
            if (j == 0 && i == 0) sb.append("?")
            else if (j == 0 && i == gridSize - 1) sb.append("?")
            else if (j == gridSize - 1 && i == 0) sb.append("?")
            else if (j == gridSize - 1 && i == gridSize - 1) sb.append("?")
            else if (j == 0 && i % (padding + 1) == 0) sb.append("?")
            else if (j == gridSize - 1 && i % (padding + 1) == 0) sb.append("?")
            else if (j % (padding + 1) == 0 && i == 0) sb.append("?")
            else if (j % (padding + 1) == 0 && i == gridSize - 1) sb.append("?")
            else if (j % (padding + 1) == 0 && i % (padding + 1) == 0) sb.append("?")
            else if (j % (padding + 1) == 0) sb.append("?")
            else if (i % (padding + 1) == 0) sb.append("?")
            else sb.append (" ")
          }
        }
      }
      sb.append (newline)
    }
    sb.toString ()
  }
}