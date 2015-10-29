package hayago

import hayago.Game._
import org.specs2.mutable._

class GameSpec extends Specification {

  sequential

  "A fresh game" should {
    "should know who's turn it is" in {
      val config = Game.Configuration (19, Player.Capulet)
      val startState = Game.State (config)
      startState.colourToPlayNext must_== Colour.Black
      startState.playerToPlayNext must_== Player.Capulet
    }
  }

  "The board" should {
    "should correctly detect groups" in {
      //val b = Game.Board.createS (19, Map (
      //  "D4" -> Colour.Black, "C4" -> Colour.Black, "E4" -> Colour.Black, "D3" -> Colour.Black, "D5" -> Colour.Black,
      //  "H1" -> Colour.White, "H2" -> Colour.White, "H3" -> Colour.White, "G3" -> Colour.White))

      val b = Game.Board.create (19, Map (
        Board.Intersection (4, 4) -> Colour.Black,
        Board.Intersection (3, 4) -> Colour.Black,
        Board.Intersection (5, 4) -> Colour.Black,
        Board.Intersection (4, 3) -> Colour.Black,
        Board.Intersection (4, 5) -> Colour.Black,
        Board.Intersection (8, 1) -> Colour.White,
        Board.Intersection (8, 2) -> Colour.White,
        Board.Intersection (8, 3) -> Colour.White,
        Board.Intersection (7, 3) -> Colour.White))

      b.groups.size must_== 2
    }
  }

  "Surrounding a stone" should {
    "should result in it's capture" in {
      val config = Game.Configuration (19, Game.Player.Capulet)
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
      beforeCapture ("D4") must_== Some (Game.Colour.White)
      beforeCapture ("C4") must_== Some (Game.Colour.Black)
      beforeCapture ("E4") must_== Some (Game.Colour.Black)
      beforeCapture ("D3") must_== Some (Game.Colour.Black)
      beforeCapture ("D5") must_== Some (Game.Colour.Black)
      beforeCapture.stones.size must_== 5

      val afterCapture = Game.State (config, turns :+ Game.Turn.play ("D5")).board
      afterCapture ("D4") must_== None
      afterCapture ("C4") must_== Some (Game.Colour.Black)
      afterCapture ("E4") must_== Some (Game.Colour.Black)
      afterCapture ("D3") must_== Some (Game.Colour.Black)
      afterCapture ("D5") must_== Some (Game.Colour.Black)
      afterCapture.stones.size must_== 5
    }
  }
}
