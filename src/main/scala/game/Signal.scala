package hayago.game

sealed trait Signal
object Signal {
  object Pass extends Signal { override def toString = "PASS" }
  object Resign extends Signal { override def toString = "RESIGN" }
  def unapply (s: String): Option[Signal] = s.toLowerCase match {
    case "pass" => Some (Signal.Pass)
    case "resign" => Some (Signal.Resign)
    case _ => None
  }
}