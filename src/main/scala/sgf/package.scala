package hayago
import hayago.game._
import scala.util.matching.Regex._

package object sgf {

  def toIntersection (sgfStr: String): Option[Intersection] = {
    import scala.util.matching.Regex._
    object char { def unapply (str: String): Option[Char] = str.toLowerCase.headOption  }
    "([a-sA-S])([a-sA-S])".r.findFirstMatchIn (sgfStr) match {
      case Some (Groups (char (x), char (y))) =>
        Some (Intersection (x.toInt - 'a'.toInt, y.toInt - 'a'.toInt))
      case _ => None
    }
  }

  def toSgfString (i: Intersection) = "" + ('a'.toInt + i.x).toChar + ('a'.toInt + i.y).toChar

  def parse (sfg: String): game.Session = {
    val size = "[^A-Z]SZ\\[(\\d+)\\]".r.findFirstMatchIn(sfg).map { case Groups (one) => one }.map (_.toInt).getOrElse (19)
    val komi = "[^A-Z]KM\\[(\\d+.\\d+)\\]".r.findFirstMatchIn(sfg).map { case Groups (one) => one }.map (_.toFloat).getOrElse (0f)
    val turns = "[^A-Z][BW]\\[([a-sA-S]{2})\\]".r.findAllMatchIn (sfg).map { case Groups (one) => one }.toList.map (toIntersection).collect { case Some (i) => i }.map (Turn.create)
    Session (Configuration(size, Player.Montague, Map(), komi), turns)
  }
}

