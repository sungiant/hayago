package hayago

object Engine {
  type Pass = Unit
  type Turn = Either[Pass, Location]

  sealed trait Player
  object Montague extends Player
  object Capulet extends Player

  sealed trait Colour
  object Black extends Colour
  object White extends Colour

  case class Location(x: Int, y: Int)

  case class GameConfiguration(boardSize: Int, firstTurn: Player, handicap: Map[Location, Player], komi: Double)

  case class GameState (setup: GameConfiguration, history: List[Turn]) {
    def boardState: List[Option[Colour]] = ???

    def boardState2DA: Array[Array[Option[Colour]]] = ???

    def isComplete: Boolean = ???
  }

}