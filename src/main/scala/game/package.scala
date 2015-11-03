package hayago

package object game {
  import hayago._
  import scala.util._
  import com.github.nscala_time.time.Imports._
  import scala.collection.immutable.HashSet
  import cats.data.{ReaderT, Reader, Kleisli}

  val firstTurnColour = Colour.Black // Black is always first
}