package hayago.game

import scala.collection.immutable.HashSet

sealed trait Signal
object Signal {
  object Pass extends Signal { override def toString = "PASS" }
  object Resign extends Signal { override def toString = "RESIGN" }
  def unapply (s: String): Option[Signal] = s match {
    case "PASS" => Some (Signal.Pass)
    case "RESIGN" => Some (Signal.Resign)
    case str => str.startsWith ("EVALUATION:") match {
      case true =>
        val deadStones = str
          .replace ("EVALUATION:", "")
          .split (",")
          .map (Intersection.unapply)
          .collect { case Some (i) => i }
        Some (Signal.Evaluation (HashSet () ++ deadStones))
      case false => None
    }
  }
  case class Evaluation (deadStones: HashSet[Intersection]) extends  Signal { override def toString = "EVALUATION:" + deadStones.toList.mkString (",") }
}
