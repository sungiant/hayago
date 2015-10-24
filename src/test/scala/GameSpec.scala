package hayago

import org.specs2.mutable._

class GameSpec extends Specification {
  "A fresh game" should {
    "should know who's turn it is" in {
      val config = Game.Configuration (19, Game.Capulet)
      val startState = Game.State (config)
      startState.colourToPlayNext must_== Game.Black
      startState.playerToPlayNext must_== Game.Capulet
    }
  }
}
