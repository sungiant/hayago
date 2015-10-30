package hayago

import org.specs2.mutable._

class GameSpec extends Specification { sequential
  import hayago.Game._
  import scala.util._

  "A fresh game" should {
    "know who's turn it is" in {
      val startState1 = Game.State (Game.Configuration.default)
      startState1.colourToPlayNext must_== Colour.Black
      startState1.playerToPlayNext must_== Player.Montague

      val startState2 = Game.State (Game.Configuration.default.copy(firstTurn = Player.Capulet))
      startState2.colourToPlayNext must_== Colour.Black
      startState2.playerToPlayNext must_== Player.Capulet
    }
  }

  "An Intersection" should {
    "unapply from strings" in {
      Intersection.unapply ("A1") must_== Some (Intersection (0, 0))
      Intersection.unapply ("C5") must_== Some (Intersection (2, 4))
      Intersection.unapply ("B10") must_== Some (Intersection (1, 9))
      Intersection.unapply ("b15") must_== Some (Intersection (1, 14))
      Intersection.unapply ("F11") must_== Some (Intersection (5, 10))
      Intersection.unapply ("-") must_== None
      Intersection.unapply ("?") must_== None
      Intersection.unapply ("1-1") must_== None
      Intersection.unapply ("a 1") must_== None
      Intersection.unapply ("B-14") must_== None
    }
    "stringify correctly" in {
      Intersection (0, 0).toString must_== "A1"
      Intersection (2, 4).toString must_== "C5"
      Intersection (1, 9).toString must_== "B10"
      Intersection (1, 14).toString must_== "B15"
      Intersection (5, 10).toString must_== "F11"
    }
  }

  "The board" should {
    "correctly detect groups" in {
      val b = Game.Board.create (19, Map (
        Intersection (4, 4) -> Colour.Black,
        Intersection (3, 4) -> Colour.Black,
        Intersection (5, 4) -> Colour.Black,
        Intersection (4, 3) -> Colour.Black,
        Intersection (4, 5) -> Colour.Black,
        Intersection (8, 1) -> Colour.White,
        Intersection (8, 2) -> Colour.White,
        Intersection (8, 3) -> Colour.White,
        Intersection (7, 3) -> Colour.White))

      b.groups.size must_== 2
    }
  }

  "Surrounding a stone" should {
    "result in it's capture" in {
      val turns =
        Game.Turn.create ("C4") ::
        Game.Turn.create ("D4") ::
        Game.Turn.create ("E4") ::
        Game.Turn.create ("-") ::
        Game.Turn.create ("D3") ::
        Game.Turn.create ("-") ::
        Nil

      val beforeCapture = Game.State (Game.Configuration.default, turns).board

      beforeCapture.stones.size must_== 4
      beforeCapture ("D4") must_== Success (Some (Game.Colour.White))
      beforeCapture ("C4") must_== Success (Some (Game.Colour.Black))
      beforeCapture ("E4") must_== Success (Some (Game.Colour.Black))
      beforeCapture ("D3") must_== Success (Some (Game.Colour.Black))
      beforeCapture ("D5") must_== Success (None)

      val afterCapture = Game.State (Game.Configuration.default, turns :+ Game.Turn.create ("D5")).board

      afterCapture.stones.size must_== 4
      afterCapture ("D4") must_== Success (None)
      afterCapture ("C4") must_== Success (Some (Game.Colour.Black))
      afterCapture ("E4") must_== Success (Some (Game.Colour.Black))
      afterCapture ("D3") must_== Success (Some (Game.Colour.Black))
      afterCapture ("D5") must_== Success (Some (Game.Colour.Black))
    }
  }
}
