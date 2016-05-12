package hayago.game

import hayago._
import scala.util._

final case class Score (komi: Float, territory: Int, captures: Int, hostages: Int) {
  lazy val total = komi + territory.toFloat + captures.toFloat + hostages.toFloat
}
