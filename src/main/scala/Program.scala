package hayago

import cats.std.all._
import com.gokgs.client.gtp._;
import java.util.Properties

object Program {
  def main (args: Array[String]) {
    val gtpClient = for {
      username <- Option (System.getenv ().get ("GTP_USERNAME"))
      password <- Option (System.getenv ().get ("GTP_PASSWORD"))
    } yield {
      val props = new Properties ();
      props.setProperty ("name", username)
      props.setProperty ("password", password)
      props.setProperty ("talk", "I'm a bot and have not yet learnt to hold a conversation...")
      props.setProperty ("mode", "auto")
      props.setProperty ("automatch.speed", "blitz,medium")
      props.setProperty ("automatch.rank", "20k")
      props.setProperty ("verbose", "t")
      val logName = "hayago_log"
      new GtpClient (System.in, System.out, new Options (props, logName))
    }

    gtpClient match {
      case Some (client) =>
        println ("About to run GTP client.")
        client.go ()
      case None =>
        println ("Failed to create GTP client.")
    }
  }
}
