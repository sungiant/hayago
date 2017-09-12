package hayago.game

final case class Score (komi: Float, territory: Int, captures: Int, hostages: Int) {
  lazy val total = komi + territory.toFloat + captures.toFloat + hostages.toFloat
}
