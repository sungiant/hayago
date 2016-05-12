package hayago.game

final case class Configuration (boardSize: Int, firstTurn: Player, handicap: Map[Intersection, Player], komi: Float)
object Configuration {
  lazy val default = Configuration (9, Player.Montague,  Map (), 5.5f)
}
