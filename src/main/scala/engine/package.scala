package hayago

package object engine {
  import cats._
  import scala.concurrent.Future
  import cats.data._

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

  def takeRandomTurn () (implicit MF: Monad[Future]): StateT[Future, game.Session, Unit] = for {
    gameSession <- ms.get
    _ <- gameSession.isComplete match {
      case true => StateT.pure[Future, game.Session, Unit] (())
      case false =>
        val turn = nextRandomTurn (gameSession)
        ms.set (gameSession.copy (history = gameSession.history :+ turn))
    }
  } yield ()

  def takeMonteCarloTurn (n: Int) (implicit MF: Monad[Future]): StateT[Future, game.Session, Unit] = for {
    gameSession <- ms.get
    _ <- gameSession.isComplete match {
      case true => StateT.pure[Future, game.Session, Unit] (())
      case false =>
        val results = (0 until n).map { _ =>
          val initialMove = nextRandomMove (gameSession)

          (initialMove, ())

        }

        val nextMove: Option [game.Intersection] = ???

        val turn = nextMove match {
          case Some (move) => game.Turn.create (move)
          case None => game.Turn.create (game.Signal.Pass)
        }

        ms.set (gameSession.copy (history = gameSession.history :+ turn))
    }
  } yield ()
}
