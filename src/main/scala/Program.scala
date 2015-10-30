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

    implicit val ex: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    implicit val MF = scalaFutureMonad

    // todo: workout how to do this with EVAL
    var gameState = Game.State (Game.Configuration.default)
    Iterator
      .continually (StdIn.readLine())
      .takeWhile { line =>
        val f = GTP.gtpLoop (line).run (gameState)
        Try (Await.result (f, 60.seconds)) match {
          case Success ((newState, GTP.GtpLoopStatus.OK)) =>
            gameState = newState
            true
          case _ => false
        }
      }
      .toList
  }

}
