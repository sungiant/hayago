package hayago

import cats.std.all._
import java.util.Properties
import cats.syntax.eq._
import scala.util._

import Engine._

object Program {
  def main (args: Array[String]): Unit = {
    import scala.io._

    println ("Hayago Engine v0.0.1")

    val exampleBoardState = Array.tabulate[Option[Colour]](9, 9) { (i, j) => (i, j) match {
      case (0, 5) => Some (White)
      case (0, 7) => Some (White)
      case (1, 6) => Some (White)
      case (2, 6) => Some (White)
      case (3, 6) => Some (White)
      case (4, 5) => Some (White)
      case (4, 7) => Some (White)
      case (5, 5) => Some (White)
      case (5, 7) => Some (White)
      case (6, 4) => Some (White)
      case (6, 6) => Some (White)
      case (7, 5) => Some (White)
      case (1, 2) => Some (Black)
      case (1, 4) => Some (Black)
      case (1, 5) => Some (Black)
      case (1, 7) => Some (Black)
      case (3, 3) => Some (Black)
      case (3, 4) => Some (Black)
      case (5, 4) => Some (Black)
      case (5, 6) => Some (Black)
      case (6, 1) => Some (Black)
      case _ => None
    }}

    PrettyPrinter.print (exampleBoardState)

    implicit val MF = scalaFutureMonad (scala.concurrent.ExecutionContext.Implicits.global)

    val record = Iterator
      .continually (StdIn.readLine())
      .takeWhile (GTP.gtpLoop)
      .toList

    println (s"record: $record")
  }

}
