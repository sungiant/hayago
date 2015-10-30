package hayago

object Engine {
  import cats._
  import scala.concurrent.Future
  import cats.state._

  def takeTurn () (implicit MF: Monad[Future]): StateT[Future, Game.State, Unit] = for {
    gameState <- ms.get
    _ <- gameState.isComplete match {
      case true => StateT.pure[Future, Game.State, Unit] (())
      case false => for {
        _ <- ms.set(gameState.copy(history = gameState.history :+ Game.Turn.create (Game.Signal.Pass)))
      } yield ()
    }
  } yield ()
}
