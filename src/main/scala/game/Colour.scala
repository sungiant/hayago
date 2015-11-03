package hayago.game

sealed trait Colour {
  val opposition = this match {
    case Colour.Black => Colour.White
    case Colour.White => Colour.Black
  }
}
object Colour {
  object Black extends Colour { override def toString = "BLACK" }
  object White extends Colour { override def toString = "WHITE" }
}