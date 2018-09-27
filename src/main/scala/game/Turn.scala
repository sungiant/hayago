package hayago.game

import com.github.nscala_time.time.Imports._

final case class Turn (action: Either[Signal, Intersection], time: DateTime) // probably should remove time from this
object Turn {
  def create (i: Intersection): Turn = Turn (Right (i), DateTime.now)

  def create (s: Signal): Turn = Turn (Left (s), DateTime.now)

  def create (action: Either[Signal, Intersection]): Turn = Turn (action, DateTime.now)

  def create (str: String): Turn = Intersection.unapply (str) match {
    case Some (i) => create (i)
    case None =>
      Signal.unapply (str) match {
        case Some (s) => create (s)
        case None => create (Signal.Pass)
      }
  }
}

trait IllegalTurnReason
object IllegalTurnReason {
  object Ko extends IllegalTurnReason
  object EvaluationExpected extends IllegalTurnReason // this happens if in the agreement phase any turn is not an evaluation signal
  object GameComplete extends IllegalTurnReason

}
case class IllegalTurnException (reason: IllegalTurnReason) extends Exception
