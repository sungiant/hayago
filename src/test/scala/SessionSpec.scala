package hayago.game.tests

import org.specs2.mutable._
import org.specs2.specification.core._
import hayago.game._
import scala.collection.immutable.HashSet

class SessionSpec extends Specification { sequential
  import scala.util._

  "A fresh game session" should {
    "know who's turn it is" in {
      val startState1 = Session (Configuration.default)
      startState1.colourToPlay must_== Colour.Black
      startState1.playerToPlay must_== Player.Montague

      val startState2 = Session (Configuration.default.copy(firstTurn = Player.Capulet))
      startState2.colourToPlay must_== Colour.Black
      startState2.playerToPlay must_== Player.Capulet
    }
  }

  "The game session" should {
    val turns =
        Turn.create ("C:4") ::
        Turn.create ("D:4") ::
        Turn.create ("C:6") ::
        Turn.create ("D:6") ::
        Turn.create ("B:5") ::
        Turn.create ("E:5") ::
        Turn.create ("D:5") ::
        Turn.create ("C:5") ::
        Nil

    "know about the game end conditions" in {
      val t0  = turns
      val t1  = turns :+ Turn.create ("RESIGN")
      val t2a = turns :+ Turn.create ("PASS")
      val t2b = t2a   :+ Turn.create ("PASS")
      val t2c = t2b   :+ Turn.create (Signal.Evaluation (HashSet ())) 
      val t2d = t2c   :+ Turn.create (Signal.Evaluation (HashSet ()))


      val s0  = Session (Configuration.default, t0)
      val s1  = Session (Configuration.default, t1)
      val s2a = Session (Configuration.default, t2a)
      val s2b = Session (Configuration.default, t2b)
      val s2c = Session (Configuration.default, t2c)
      val s2d = Session (Configuration.default, t2d)

      s0.isAgreementPhase must_== false
      s0.isComplete must_== false

      s1.isAgreementPhase must_== false
      s1.isComplete must_== true

      s2a.isAgreementPhase must_== false
      s2a.isComplete must_== false

      s2b.isAgreementPhase must_== true
      s2b.isComplete must_== false

      s2c.isAgreementPhase must_== true
      s2c.isComplete must_== false

      s2d.isAgreementPhase must_== false
      s2d.isComplete must_== true


      val turn = Turn.create ("F:8")

      s0.applyTurn (turn).isSuccess must_== true
      s1.applyTurn (turn) must_== Failure (IllegalTurnException (IllegalTurnReason.GameComplete))
      s2a.applyTurn (turn).isSuccess must_== true
      s2b.applyTurn (turn) must_== Failure (IllegalTurnException (IllegalTurnReason.EvaluationExpected))
      s2c.applyTurn (turn) must_== Failure (IllegalTurnException (IllegalTurnReason.EvaluationExpected))
      s2d.applyTurn (turn) must_== Failure (IllegalTurnException (IllegalTurnReason.GameComplete))
    }

    "know about illegal moves due to Ko" in {
      val s = Session (Configuration.default, turns)
      val b = s.currentBoard
      b.stones.size must_== 7
      b ("C:4") must_== Success (Some (Colour.Black))
      b ("D:4") must_== Success (Some (Colour.White))
      b ("C:6") must_== Success (Some (Colour.Black))
      b ("D:6") must_== Success (Some (Colour.White))
      b ("B:5") must_== Success (Some (Colour.Black))
      b ("E:5") must_== Success (Some (Colour.White))
      b ("D:5") must_== Success (None)
      b ("C:5") must_== Success (Some (Colour.White))

      s.applyTurn (Turn.create ("D:5")) must_== Failure (IllegalTurnException (IllegalTurnReason.Ko))
      s.applyTurn (Turn.create ("D:8"))
        .flatMap (_.applyTurn (Turn.create ("F:8")))
        .flatMap (_.applyTurn (Turn.create ("D:5"))) match { case Success (sn) => ok; case _ => ko }

      s.applyTurn (Turn.create ("PASS"))
        .flatMap (_.applyTurn (Turn.create ("F:8")))
        .flatMap (_.applyTurn (Turn.create ("D:5"))) match { case Success (sn) => ok; case _ => ko }
    }


    val examples = Data.example9x9 :: Data.second_hayago_championship :: Nil

    Fragment.foreach (examples) { example =>

      s"When scoring ${example.description}, the game session" should {
        "calculate black captures correctly" in {
          example.completeSession.captureCount(Colour.Black) must_== example.expectedScoreBlack.captures
        }

        "calculate white captures correctly" in {
          example.completeSession.captureCount(Colour.White) must_== example.expectedScoreWhite.captures
        }

        val scores = example.completeSession.score (example.expectedDeadStones).get

        "calculate black score correctly, given a list of dead stones" in {
          scores (example.completeSession.player (Colour.Black)) must_== example.expectedScoreBlack
        }

        "calculate white score correctly, given a list of dead stones" in {
          scores (example.completeSession.player (Colour.White)) must_== example.expectedScoreWhite
        }
      }
    }
  }
}
