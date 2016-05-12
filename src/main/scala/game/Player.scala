package hayago.game

import cats._

sealed trait Player
object Player {
  def opposition (player: Player) = player match {
    case Montague => Capulet
    case Capulet => Montague
  }
  object Montague extends Player { override def toString = "MONTAGUE" }
  object Capulet extends Player { override def toString = "CAPULET" }

  implicit val eq = new Eq[Player] {
    def eqv (x: Player, y: Player): Boolean = x == y
  }
}
