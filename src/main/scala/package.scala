package object hayago {
  import scala.concurrent.Future
  import cats.Monad
  import cats.mtl.MonadState
  import cats.data.StateT
  import cats.mtl.implicits._

  def ms (implicit MF: Monad[Future]) = MonadState[({type ST[Z] = StateT[Future, game.Session, Z]})#ST, game.Session]

  val newLine = System.getProperty ("line.separator")

  implicit def ToIdOps[A](a: A): IdOps[A] = new IdOps(a)
}
