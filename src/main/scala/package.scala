package object hayago {
  import scala.concurrent._
  import cats._

  def scalaFutureMonad (implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
    override def pure[A] (a: A): Future[A] = Future (a)
    override def flatMap[A, B] (fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap (a => f (a))
    override def ap[A, B] (fa: Future[A])(ff: Future[A => B]): Future[B] = fa.flatMap (a => ff.map (f => f (a)))
  }
}