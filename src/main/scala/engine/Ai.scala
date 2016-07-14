package hayago.engine

import cats.Monad
import cats.data._
import hayago.game._
import hayago.game.Intersection

import scala.collection.immutable.HashSet
import scala.concurrent.Future

trait Ai {
  def takeTurn () (implicit MF: Monad[Future]): StateT[Future, Session, Unit]
}
