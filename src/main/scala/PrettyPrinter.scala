package hayago

import scala.util._
import scala.text._
import cats.std.all._
import cats.syntax.all._

object PrettyPrinter {
  private val newline = System.getProperty("line.separator")

  def print (board: Game.Board): Unit = println (stringify (board))
  def stringify (board: Game.Board): String = {
    val stonePadding = 2
    val stoneSize = 1 + (2* stonePadding)
    val gridPadding = 1
    val padding = (stonePadding * 2) + gridPadding

    assert (gridPadding >= 1)
    assert (stoneSize % 2 != 0)
    assert (stoneSize <= padding)

    val sb = new StringBuilder()
    val gridSize = board.size + ((board.size - 1) * padding)

    for (j <- -padding until gridSize + padding) {
      for (i <- -padding until gridSize + padding) {
        val point: Option[Game.Colour] =
          if ((j >= - stonePadding && j <= gridSize + stonePadding) && (i >= - stonePadding && i <= gridSize + stonePadding)) {
            val aj = j % (padding + 1)
            val ai = i % (padding + 1)
            val jjo = if (aj <= stonePadding) Some (j - aj) else if (aj > padding - stonePadding) Some (j + aj) else None
            val iio = if (ai <= stonePadding) Some (i - ai) else if (ai > padding - stonePadding) Some (i + ai) else None
            (jjo, iio) match {
              case (Some (jj), Some (ii)) =>
                val y = jj / (padding + 1)
                val x = ii / (padding + 1)
                val point = Game.Board.Intersection (x, y)
                board (point) match {
                  case Success (Some (p)) =>
                    val aj = if (j < 0) (j + padding + 1) % (padding + 1) else j % (padding + 1)
                    val ai = if (i < 0) (i + padding + 1) % (padding + 1) else i % (padding + 1)
                    val iEdge = ai == stonePadding || ai == padding - stonePadding + 1
                    val jEdge = aj == stonePadding || aj == padding - stonePadding + 1
                    if (jEdge && iEdge) None else Some (p)
                  case _ => None
                }
              case _ => None
            }
          } else None

        sb.append (point match {
          case Some (p) if p == Game.Black => "▓"
          case Some (p) if p == Game.White => "░"
          case None =>
            if (j < 0 || j > gridSize - 1 || i < 0 || i > gridSize - 1) " "
            else {
              if (j == 0 && i == 0) "┏"
              else if (j == 0 && i == gridSize - 1) "┓"
              else if (j == gridSize - 1 && i == 0) "┗"
              else if (j == gridSize - 1 && i == gridSize - 1) "┛"
              else if (j == 0 && i % (padding + 1) == 0) "┳"
              else if (j == gridSize - 1 && i % (padding + 1) == 0) "┻"
              else if (j % (padding + 1) == 0 && i == 0) "┣"
              else if (j % (padding + 1) == 0 && i == gridSize - 1) "┫"
              else if (j % (padding + 1) == 0 && i % (padding + 1) == 0) "╋"
              else if (j % (padding + 1) == 0) "━"
              else if (i % (padding + 1) == 0) "┃"
              else " "
            }
        })
      }
      sb.append (newline)
    }
    sb.toString ()
  }
}