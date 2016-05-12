package hayago.game

import com.github.nscala_time.time.Imports._

final case class Turn (action: Either[Signal, Intersection], time: DateTime)
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
