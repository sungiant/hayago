package object hayago {
  import scala.concurrent.{Future, ExecutionContext}
  import cats.{Monad, MonadState}
  import cats.data.StateT

  def scalaFutureMonad (implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
    override def pure[A] (a: A): Future[A] = Future (a)
    override def flatMap[A, B] (fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap (a => f (a))
    override def ap[A, B] (ff: Future[A => B])(fa: Future[A]): Future[B] = fa.flatMap (a => ff.map (f => f (a)))
  }

  def ms (implicit MF: Monad[Future]) = MonadState[({type ST[Z] = StateT[Future, game.Session, Z]})#ST, game.Session]

  val newLine = System.getProperty ("line.separator")

  implicit def ToIdOps[A](a: A): IdOps[A] = new IdOps(a)
}
