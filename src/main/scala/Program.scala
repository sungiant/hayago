package hayago

import cats.std.all._
import java.util.Properties
import cats.syntax.eq._
import scala.util._
import scala.concurrent._
import scala.concurrent.duration._

object Program {
  def main (args: Array[String]): Unit = {
    import scala.io._

    println ("Hayago Engine v0.0.1")

    implicit val ex: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    implicit val MF = scalaFutureMonad

    // todo: workout how to do this with EVAL
    var gameState = game.Session (game.Configuration.default)
    Iterator
      .continually (StdIn.readLine())
      .takeWhile { line =>
        val f = gtp.Protocol.process (line).run (gameState)
        Try (Await.result (f, 60.seconds)) match {
          case Success ((newState, gtp.ProtocolStatus.OK)) =>
            gameState = newState
            true
          case _ => false
        }
      }
      .toList
  }

}
