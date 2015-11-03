package hayago

package object engine {
  import cats._
  import scala.concurrent.Future
  import cats.state._

  def takeTurn () (implicit MF: Monad[Future]): StateT[Future, game.State, Unit] = for {
    gameState <- ms.get
    _ <- gameState.isComplete match {
      case true => StateT.pure[Future, game.State, Unit] (())
      case false =>
        val x = scala.util.Random.nextInt (gameState.setup.boardSize)
        val y = scala.util.Random.nextInt (gameState.setup.boardSize)
        val i = game.Intersection (x, y)
        val randomTurn = game.Turn.create (i)
        val passTurn = game.Turn.create (game.Signal.Pass)
        val turn = gameState.isTurnLegal (randomTurn) match {
          case true => randomTurn
          case false => passTurn
        }
        ms.set (gameState.copy (history = gameState.history :+ turn))
    }
  } yield ()
}
