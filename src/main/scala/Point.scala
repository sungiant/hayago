package hayago

// counting from zero
case class Point (x: Int, y: Int) {
  override def toString = if (x < 26) ('A'.toInt + x).toChar + (y + 1).toString else toString
}
object Point {
  def unapply (str: String): Option[Point] = {
    import scala.util.matching.Regex._
    import scala.util._
    object int { def unapply (str: String): Option[Int] = Try(str.toInt).toOption }
    object char { def unapply (str: String): Option[Char] = str.headOption }
    "([A-Za-z])([0-9]{1,2})".r.findFirstMatchIn (str) match {
      case Some (Groups (char (c), int (n))) => Some (Point (c.toInt - 1, n - 1))
      case _ => None
    }
  }
}
