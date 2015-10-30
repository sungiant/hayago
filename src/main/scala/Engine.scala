package hayago

object Engine {
  import cats._
  import scala.concurrent.Future
  import cats.state._

  def takeTurn () (implicit MF: Monad[Future]): StateT[Future, Game.State, Unit] = for {
    gameState <- ms.get
    _ <- gameState.isComplete match {
      case true => StateT.pure[Future, Game.State, Unit] (())
      case false =>
        val x = scala.util.Random.nextInt (gameState.setup.boardSize)
        val y = scala.util.Random.nextInt (gameState.setup.boardSize)
        val i = Game.Intersection (x, y)
        val randomTurn = Game.Turn.create (i)
        val passTurn = Game.Turn.create (Game.Signal.Pass)
        val turn = gameState.isTurnLegal (randomTurn) match {
          case true => randomTurn
          case false => passTurn
        }
        ms.set (gameState.copy (history = gameState.history :+ turn))
    }
  } yield ()
}
