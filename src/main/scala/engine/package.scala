package hayago

package object engine {
  import cats._
  import scala.concurrent.Future
  import cats.state._

  private def nextRandomMove (gameState: game.State): Option[game.Intersection] = {
    val possibleIntersections = gameState.legalLocationsForNextTurn.toList
    possibleIntersections.isEmpty match {
      case true => None
      case false =>
        val idx = scala.util.Random.nextInt (possibleIntersections.length)
        val i = possibleIntersections (idx)
        Some (i)
    }
  }

  private def nextRandomTurn (gameState: game.State): game.Turn = nextRandomMove (gameState) match {
    case Some (i) => game.Turn.create (i)
    case None => game.Turn.create (game.Signal.Pass)
  }


  def takeRandomTurn () (implicit MF: Monad[Future]): StateT[Future, game.State, Unit] = for {
    gameState <- ms.get
    _ <- gameState.isComplete match {
      case true => StateT.pure[Future, game.State, Unit] (())
      case false =>
        val turn = nextRandomTurn (gameState)
        ms.set (gameState.copy (history = gameState.history :+ turn))
    }
  } yield ()


  def takeMonteCarloTurn (n: Int) (implicit MF: Monad[Future]): StateT[Future, game.State, Unit] = for {
    gameState <- ms.get
    _ <- gameState.isComplete match {
      case true => StateT.pure[Future, game.State, Unit] (())
      case false =>

        val results = (0 until n).map { _ =>
          val initialMove = nextRandomMove (gameState)
          (initialMove, ())
        }

        ???

        //ms.set (gameState.copy (history = gameState.history :+ turn))
    }
  } yield ()
}
