package hayago

import org.specs2.mutable._

class GameSpec extends Specification { sequential
  import hayago.Game._
  import scala.util._

  "A fresh game" should {
    "know who's turn it is" in {
      val config = Game.Configuration (19, Player.Capulet)
      val startState = Game.State (config)
      startState.colourToPlayNext must_== Colour.Black
      startState.playerToPlayNext must_== Player.Capulet
    }
  }

  "An Intersection" should {
    "unapply from strings" in {
      Board.Intersection.unapply ("A1") must_== Some (Board.Intersection (0, 0))
      Board.Intersection.unapply ("C5") must_== Some (Board.Intersection (2, 4))
      Board.Intersection.unapply ("B10") must_== Some (Board.Intersection (1, 9))
      Board.Intersection.unapply ("b15") must_== Some (Board.Intersection (1, 14))
      Board.Intersection.unapply ("F11") must_== Some (Board.Intersection (5, 10))
      Board.Intersection.unapply ("-") must_== None
      Board.Intersection.unapply ("?") must_== None
      Board.Intersection.unapply ("1-1") must_== None
      Board.Intersection.unapply ("a 1") must_== None
      Board.Intersection.unapply ("B-14") must_== None
    }
    "stringify correctly" in {
      Board.Intersection (0, 0).toString must_== "A1"
      Board.Intersection (2, 4).toString must_== "C5"
      Board.Intersection (1, 9).toString must_== "B10"
      Board.Intersection (1, 14).toString must_== "B15"
      Board.Intersection (5, 10).toString must_== "F11"
    }
  }

  "The board" should {
    "correctly detect groups" in {
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
    "result in it's capture" in {
      val config = Game.Configuration (19, Game.Player.Capulet)
      val turns =
        Game.Turn.play ("C4") ::
        Game.Turn.play ("D4") ::
        Game.Turn.play ("E4") ::
        Game.Turn.play ("-") ::
        Game.Turn.play ("D3") ::
        Game.Turn.play ("-") ::
        Nil

      val beforeCapture = Game.State (config, turns).board

      beforeCapture.stones.size must_== 4
      beforeCapture ("D4") must_== Success (Some (Game.Colour.White))
      beforeCapture ("C4") must_== Success (Some (Game.Colour.Black))
      beforeCapture ("E4") must_== Success (Some (Game.Colour.Black))
      beforeCapture ("D3") must_== Success (Some (Game.Colour.Black))
      beforeCapture ("D5") must_== Success (None)

      val afterCapture = Game.State (config, turns :+ Game.Turn.play ("D5")).board

      afterCapture.stones.size must_== 4
      afterCapture ("D4") must_== Success (None)
      afterCapture ("C4") must_== Success (Some (Game.Colour.Black))
      afterCapture ("E4") must_== Success (Some (Game.Colour.Black))
      afterCapture ("D3") must_== Success (Some (Game.Colour.Black))
      afterCapture ("D5") must_== Success (Some (Game.Colour.Black))
    }
  }
}
