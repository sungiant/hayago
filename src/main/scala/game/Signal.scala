package hayago.game

sealed trait Signal
object Signal {
  object Pass extends Signal { override def toString = "PASS" }
  object Resign extends Signal { override def toString = "RESIGN" }
  def unapply (s: String): Option[Signal] = s match {
    case "PASS" => Some (Signal.Pass)
    case "RESIGN" => Some (Signal.Resign)
    case _ => None
  }
}
