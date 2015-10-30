package object hayago {
  import scala.concurrent.{Future, ExecutionContext}
  import cats.{Monad, MonadState}
  import cats.state.StateT

  def scalaFutureMonad (implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
    override def pure[A] (a: A): Future[A] = Future (a)
    override def flatMap[A, B] (fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap (a => f (a))
    override def ap[A, B] (fa: Future[A])(ff: Future[A => B]): Future[B] = fa.flatMap (a => ff.map (f => f (a)))
  }

  def ms (implicit MF: Monad[Future]) = MonadState[({type ST[X, Y] = StateT[Future, X, Y]})#ST, Game.State]

  val newline = System.getProperty("line.separator")

  /*
  sealed class IdOps[A](self: A) {
    final def ??(d: => A)(implicit ev: Null <:< A): A = if (self == null) d else self
    final def |>[B](f: A => B): B = f(self)
    final def <|(f: A => Any): A = { f (self); self }
    final def squared: (A, A) = (self, self)
  }

  trait ToIdOps {
    implicit def ToIdOps[A](a: A): IdOps[A] = new IdOps(a)
  }
  */
}