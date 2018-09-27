package hayago

package object engine {

  import cats._
  import scala.concurrent.Future
  import cats.data._
  import game._

  // Generates a legal random next turn for the session (if there is one) (this includes passing).
  private val randomTurn: Reader[Session, Option[Turn]] = Reader { gameSession: Session =>
    (gameSession.possibleTurns, gameSession.isComplete) match {
      case (Nil, _) => None
      case (_, true) => None
      case _ =>
        val idx = scala.util.Random.nextInt (gameSession.possibleTurns.length)
        Some (gameSession.possibleTurns (idx))
    }
  }

  //
  // Random AI
  // ---------
  // How it works:
  //   #1 if the game is already complete => no state is changed.
  //   #2 if the opponent's made a move previously and that move was a PASS => the AI issues a PASS
  //   #3 otherwise
  //     #A if there is at least one legal move available that could be made => the AI makes a play in a random legal position.
  //     #B otherwise => the AI issues a PASS
  //
  def takeRandomTurn () (implicit MF: Monad[Future]): StateT[Future, Session, Unit] = for {
    gameSession <- ms.get
    u <- (gameSession.history.reverse.headOption, gameSession.isComplete) match { 
/*#1*/case (_, true) => StateT.pure[Future, Session, Unit] (())
/*#2*/case (Some (Turn (Left (Signal.Pass), _)), _) =>
        ms.set (gameSession.copy (history = gameSession.history :+ Turn.create (Signal.Pass)))
/*#3*/case _ => randomTurn.run (gameSession) match {
/*#A*/  case Some (turn) =>
          ms.set (gameSession.copy (history = gameSession.history :+ turn))
/*#B*/  case None =>
          ms.set (gameSession.copy (history = gameSession.history :+ Turn.create (Signal.Pass)))
      }
    }
  } yield u

  //
  // Monte Carlo AI
  // --------------
  //
  def takeMonteCarloTurn (n: Int) (implicit MF: Monad[Future]): StateT[Future, Session, Unit] = for {
    gameSession <- ms.get

    possibleIntersections = gameSession.currentBoard.legalNextMovesFor (gameSession.colourToPlay).toList

    u <- (gameSession.isComplete, possibleIntersections.isEmpty) match {
        case (false, false) =>
          ???

        case (false, true) =>
          val turn = Turn.create (Signal.Pass)
          ms.set (gameSession.copy (history = gameSession.history :+ turn))
        case _ => StateT.pure[Future, Session, Unit] (())
    }
  } yield u
}
