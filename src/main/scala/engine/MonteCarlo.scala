package hayago.engine

import hayago.game._

case class MonteCarlo (
  parent: Option[MonteCarlo],
  children: List[MonteCarlo],
  // DATA
  turn: Option[Turn],
  N: Int,
  W: Int) {
  import MonteCarlo._
  lazy val isRoot: Boolean = !parent.isDefined

  lazy val U: Double = parent match {
    case None => 0.0
    case Some (p) =>
      (W.toDouble / N.toDouble) + (C * Math.sqrt (MonteCarlo.ln (p.N.toDouble) / N.toDouble))
  }
}
object MonteCarlo {
  def create () = MonteCarlo (None, Nil, None, 0, 0)
  def create (parent: MonteCarlo) = MonteCarlo (Some (parent), Nil, None, 0, 0)

  private val C: Double = 1.0
  private val ln = (x: Double) => Math.log (x) / Math.log (2.0)
}



