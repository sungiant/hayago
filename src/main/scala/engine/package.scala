package hayago

package object engine {

  import cats._
  import scala.concurrent.Future
  import cats.data._
  import game._

  private def nextRandomMove (gameSession: game.Session): Option[game.Intersection] = {
    val possibleIntersections = gameSession.currentBoard.legalNextMovesFor (gameSession.colourToPlay).toList
    possibleIntersections.isEmpty match {
      case true => None
      case false =>
        val idx = scala.util.Random.nextInt (possibleIntersections.length)
        val i = possibleIntersections (idx)
        Some (i)
    }
  }

  private def nextRandomTurn (gameSession: game.Session): game.Turn = nextRandomMove (gameSession) match {
    case Some (i) => game.Turn.create (i)
    case None => game.Turn.create (game.Signal.Pass)
  }

  //
  // Random AI
  // ---------
  // How it works:
  //   * if the opponent's made a move previously and that move was a PASS:
  //     - the AI issues a PASS
  //   * otherwise if there is at least one legal move available that could be made:
  //     - the AI makes a play in a random legal position.
  //   * otherwise:
  //    - the AI issues a PASS
  //
  def takeRandomTurn () (implicit MF: Monad[Future]): StateT[Future, game.Session, Unit] = for {
    gameSession <- ms.get
    _ <- (gameSession.history.reverse.headOption, gameSession.isComplete) match {
      case (Some (Turn (Left (Signal.Pass), _)), _) => StateT.pure[Future, game.Session, Unit] (())
      case (_, true) => StateT.pure[Future, game.Session, Unit] (())
      case _ =>
        val turn = nextRandomTurn (gameSession)
        ms.set (gameSession.copy (history = gameSession.history :+ turn))
    }
  } yield ()

  //
  // Monte Carlo AI
  // --------------
  //
  def takeMonteCarloTurn (n: Int) (implicit MF: Monad[Future]): StateT[Future, game.Session, Unit] = for {
    gameSession <- ms.get
    _ <- gameSession.isComplete match {
      case true => StateT.pure[Future, game.Session, Unit] (())
      case false =>
        /*val results = */(0 until n).map { _ =>
          val initialMove = nextRandomMove (gameSession)
          (initialMove, ())
        }

        val nextMove: Option [game.Intersection] = None

        val turn = nextMove match {
          case Some (move) => game.Turn.create (move)
          case None => game.Turn.create (game.Signal.Pass)
        }

        ms.set (gameSession.copy (history = gameSession.history :+ turn))
    }
  } yield ()
}
