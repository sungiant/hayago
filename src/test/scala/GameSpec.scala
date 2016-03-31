package hayago.game

import org.specs2.mutable._

class GameSpec extends Specification { sequential
  import hayago._
  import scala.util._

  "A fresh game" should {
    "know who's turn it is" in {
      val startState1 = State (Configuration.default)
      startState1.colourToPlayNext must_== Colour.Black
      startState1.playerToPlayNext must_== Player.Montague

      val startState2 = State (Configuration.default.copy(firstTurn = Player.Capulet))
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
      val b = Board.create (19, Map (
        Intersection (4, 4) -> Colour.Black,
        Intersection (3, 4) -> Colour.Black,
        Intersection (5, 4) -> Colour.Black,
        Intersection (4, 3) -> Colour.Black,
        Intersection (4, 5) -> Colour.Black,
        Intersection (8, 1) -> Colour.White,
        Intersection (8, 2) -> Colour.White,
        Intersection (8, 3) -> Colour.White,
        Intersection (7, 3) -> Colour.White,
        Intersection (11, 11) -> Colour.White,
        Intersection (11, 12) -> Colour.White,
        Intersection (12, 11) -> Colour.White,
        Intersection (12, 12) -> Colour.White))

      b.groups (Colour.White).size must_== 2
      b.groups (Colour.Black).size must_== 1
    }
    "know the rules of capture" in {
      val bT = Board.createS (19, Map (
        "D4" -> Colour.White,
        "C4" -> Colour.Black,
        "E4" -> Colour.Black,
        "D3" -> Colour.Black)).applyPlay ("D5", Colour.Black)

      bT match {
        case Success (b) =>
        b.stones.size must_== 4
          b ("D4") must_== Success (None)
          b ("C4") must_== Success (Some (Colour.Black))
          b ("E4") must_== Success (Some (Colour.Black))
          b ("D3") must_== Success (Some (Colour.Black))
          b ("D5") must_== Success (Some (Colour.Black))
        case _ => ko
      }
    }
  }

  "The game state" should {
    val turns =
        Turn.create ("C4") ::
        Turn.create ("D4") ::
        Turn.create ("C6") ::
        Turn.create ("D6") ::
        Turn.create ("B5") ::
        Turn.create ("E5") ::
        Turn.create ("D5") ::
        Turn.create ("C5") ::
        Nil


    "know about the game end conditions" in {
      val t0 = turns
      val t1 = turns :+ Turn.create ("pass") :+ Turn.create ("pass")
      val t2 = turns :+ Turn.create ("resign")

      val s0 = State (Configuration.default, turns)
      val s1 = State (Configuration.default, t1)
      val s2 = State (Configuration.default, t2)

      s0.isComplete must_== false
      s1.isComplete must_== true
      s2.isComplete must_== true

      val turn = Turn.create ("F8")

      s0.applyTurn (turn) match { case Success (sn) => ok; case _ => ko }
      s1.applyTurn (turn) must_== Failure (State.GameAlreadyOverException)
      s2.applyTurn (turn) must_== Failure (State.GameAlreadyOverException)
    }

    "know about illegal moves due to Ko" in {
      val s = State (Configuration.default, turns)
      val b = s.board
      b.stones.size must_== 7
      b ("C4") must_== Success (Some (Colour.Black))
      b ("D4") must_== Success (Some (Colour.White))
      b ("C6") must_== Success (Some (Colour.Black))
      b ("D6") must_== Success (Some (Colour.White))
      b ("B5") must_== Success (Some (Colour.Black))
      b ("E5") must_== Success (Some (Colour.White))
      b ("D5") must_== Success (None)
      b ("C5") must_== Success (Some (Colour.White))

      s.applyTurn (Turn.create ("D5")) must_== Failure (State.IllegalMoveDueToKoException)
      s.applyTurn (Turn.create ("D8"))
        .flatMap (_.applyTurn (Turn.create ("F8")))
        .flatMap (_.applyTurn (Turn.create ("D5"))) match { case Success (sn) => ok; case _ => ko }

      s.applyTurn (Turn.create ("pass"))
        .flatMap (_.applyTurn (Turn.create ("F8")))
        .flatMap (_.applyTurn (Turn.create ("D5"))) match { case Success (sn) => ok; case _ => ko }
    }
  }
}
