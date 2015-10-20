package hayago

import cats.std.all._
import java.util.Properties
import cats.syntax.eq._
import scala.util._
import scala.concurrent._
import scala.concurrent.duration._

import Engine._

object Program {
  def main (args: Array[String]): Unit = {
    import scala.io._

    println ("Hayago Engine v0.0.1")

    val exampleBoardState = Array.tabulate[Option[Game.Colour]](9, 9) { (i, j) => (i, j) match {
      case (0, 5) => Some (Game.White)
      case (0, 7) => Some (Game.White)
      case (1, 6) => Some (Game.White)
      case (2, 6) => Some (Game.White)
      case (3, 6) => Some (Game.White)
      case (4, 5) => Some (Game.White)
      case (4, 7) => Some (Game.White)
      case (5, 5) => Some (Game.White)
      case (5, 7) => Some (Game.White)
      case (6, 4) => Some (Game.White)
      case (6, 6) => Some (Game.White)
      case (7, 5) => Some (Game.White)
      case (1, 2) => Some (Game.Black)
      case (1, 4) => Some (Game.Black)
      case (1, 5) => Some (Game.Black)
      case (1, 7) => Some (Game.Black)
      case (3, 3) => Some (Game.Black)
      case (3, 4) => Some (Game.Black)
      case (5, 4) => Some (Game.Black)
      case (5, 6) => Some (Game.Black)
      case (6, 1) => Some (Game.Black)
      case _ => None
    }}

    //PrettyPrinter.print (exampleBoardState)

    implicit val ex: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    implicit val MF = scalaFutureMonad

    // todo: workout how to do this with EVAL
    var gameState = Game.State ()
    Iterator
      .continually (StdIn.readLine())
      .takeWhile { line =>
        val f = GTP.gtpLoop (line).run (gameState)
        Try (Await.result (f, 60.seconds)) match {
          case Success ((newState, GTP.OK)) =>
            gameState = newState
            true
          case _ =>
            false
        }
      }
      .toList
  }

}
