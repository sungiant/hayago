package hayago.game

import cats._

sealed trait Colour {
  lazy val opposition = this match {
    case Colour.Black => Colour.White
    case Colour.White => Colour.Black
  }
}
object Colour {
  object Black extends Colour { override def toString = "BLACK" }
  object White extends Colour { override def toString = "WHITE" }

  implicit val eq = new Eq[Colour] {
    def eqv (x: Colour, y: Colour): Boolean = x == y
  }
}
