package hayago.gtp

import hayago._
import scala.util._

// An int is an unsigned integer in the interval  $0 <= x <= 2^{31} - 1$.
object GtpInt { def unapply (str: String): Option[Int] = Try(str.toInt).toOption }

// A float is a floating point number representable by a 32 bit IEEE 754 float.
object GtpFloat { def unapply (str: String): Option[Float] = Try(str.toFloat).toOption }

// A string is a sequence of printable, non-whitespace characters. Strings are case sensitive.
object GtpString { def unapply (str: String): Option[String] = if (str.isEmpty) None else Some (str) }

// A vertex is a board coordinate consisting of one letter and one number, as defined in section 2.11,
// or the string ``pass''. Vertices are not case sensitive. Examples: ``B13'', ``j11''.
object GtpVertex { def unapply (str: String): Option[GtpVertex] = str match {
  case "pass" => Some (Left (game.Signal.Pass))
  case "resign" => Some (Left (game.Signal.Resign))
  case x => toIntersection (x) match {
    case Some (i) => Some (Right (i))
    case _ => None
  }
}}

// A color is one of the strings ``white'' or ``w'' to denote white, or ``black'' or ``b'' to denote black.
// Colors are not case sensitive.
object GtpColour { def unapply (str: String): Option[game.Colour] = str.toLowerCase match {
  case "white" | "w" => Some (game.Colour.White)
  case "black" | "b" => Some (game.Colour.Black)
}}

// A boolean is one of the strings ``false'' and ``true''.
object GtpBoolean { def unapply (str: String): Option[Boolean] = str.toLowerCase match {
  case "false" => Some (false)
  case "true" => Some (true)
}}
