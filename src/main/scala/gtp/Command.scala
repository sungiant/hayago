package hayago.gtp

import hayago._
import scala.util._

sealed case class Command (id: Option[Int], cmd: String, args: List[String]) {
  override def toString = {
    val i = id match {
      case Some (i) => s"[$i] "
      case None => ""
    }
    val a = args.isEmpty match {
      case true => ""
      case false => " " + args.mkString (" ")
    }
    i + cmd + a + newLine
  }
}
object Command {
  object ID { def unapply(str: String): Option[Int] = Try(str.toInt).toOption }
  def unapply(str: String): Option[Command] = str.split("\\s+").toList match {
    case ID(id) :: cmd :: args => Some(Command(Some(id), cmd, args))
    case cmd :: args => Some(Command(None, cmd, args))
    case _ => None
  }
}