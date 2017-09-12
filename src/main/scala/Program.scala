package hayago

import cats.std.all._
import java.util.Properties

import cats.syntax.eq._
import hayago.gtp.ProtocolStatus

import scala.util._
import scala.concurrent._
import scala.concurrent.duration._

object Program {
  def main (args: Array[String]): Unit = {

    type In = () => String
    type Out = (String) => Unit
    val systemOut: Out = println

    val (sockOpt, in, commandOut, responseOut): (Option[java.net.Socket], In, Out, Out) = args.contains ("--kgs") match {
      case true =>
        val s = new java.net.Socket(java.net.InetAddress.getByName("localhost"), 9999)
        val i = new scala.io.BufferedSource(s.getInputStream).getLines()
        val o = new java.io.PrintStream(s.getOutputStream)
        (Some (s), () => i.next, (str: String) => { o.print(str); o.flush(); println (str) }, println)
      case false =>
        (None, () => io.StdIn.readLine, println, println)
    }

    implicit val ex: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    implicit val MF = scalaFutureMonad

    // todo: workout how to do this with EVAL
    var gameState = game.Session (game.Configuration.default)
    Iterator
      .continually (in ())
      .takeWhile { line =>
        gtp.Protocol.parse (line) match {
          case None => true
          case Some (command) =>
            commandOut (command.toString)
            val f = gtp.Protocol.process (command).run (gameState)
            Try (Await.result (f, 60.seconds)) match {
              case Success ((newState, (gtp.ProtocolStatus.Continue, response))) =>
                responseOut (response.toString)
                gameState = newState
                true
              case Success ((_, (_, response))) =>
                responseOut (response.toString)
                false
              case _ =>
                systemOut ("Failed to process command.")
                false
            }
        }
      }
      .toList

    sockOpt.foreach (_.close ())
  }

}

