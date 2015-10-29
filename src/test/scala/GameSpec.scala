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
  "Surrounding a stone" should {
    "should result in it's capture" in {
      val config = Game.Configuration (19, Game.Capulet)
      val turns =
        Game.Turn.play ("D4") ::
        Game.Turn.play ("-") ::
        Game.Turn.play ("C4") ::
        Game.Turn.play ("-") ::
        Game.Turn.play ("E4") ::
        Game.Turn.play ("-") ::
        Game.Turn.play ("D3") ::
        Game.Turn.play ("-") ::
        Nil

      val beforeCapture = Game.State (config, turns).board
      beforeCapture ("D4") must_== Some (Game.White)
      beforeCapture ("C4") must_== Some (Game.Black)
      beforeCapture ("E4") must_== Some (Game.Black)
      beforeCapture ("D3") must_== Some (Game.Black)
      beforeCapture ("D5") must_== Some (Game.Black)
      beforeCapture.stones.size must_== 5

      val afterCapture = Game.State (config, turns :+ Game.Turn.play ("D5")).board
      afterCapture ("D4") must_== None
      afterCapture ("C4") must_== Some (Game.Black)
      afterCapture ("E4") must_== Some (Game.Black)
      afterCapture ("D3") must_== Some (Game.Black)
      afterCapture ("D5") must_== Some (Game.Black)
      afterCapture.stones.size must_== 5
    }
  }
/*
  "The board" should {
    "should correctly detect groups" in {
    val config = Game.Configuration (19, Game.Capulet)
    val turns =
    Game.Turn.play ("D4") ::
    Game.Turn.play ("H1") ::
    Game.Turn.play ("C4") ::
    Game.Turn.play ("H2") ::
    Game.Turn.play ("E4") ::
    Game.Turn.play ("J4") ::
    Game.Turn.play ("D3") ::
    Game.Turn.play ("J5") ::
    Game.Turn.play ("D5") ::
    Game.Turn.play ("J6") ::
    Nil

    Game.State (config, turns).board.groups.size must_== 3
    }
  }
  */
}
