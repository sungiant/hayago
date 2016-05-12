package hayago.game.tests

import org.specs2.mutable._
import hayago.game._

class BoardSpec extends Specification { sequential
  import scala.util._

  "The board" should {
    "correctly detect groups" in {
      val b = Board.create (19, Map (
        Intersection (4, 4) -> Colour.Black,
        Intersection (3, 4) -> Colour.Black,
        Intersection (5, 4) -> Colour.Black,
        Intersection (4, 3) -> Colour.Black,
        Intersection (4, 5) -> Colour.Black,
        Intersection (8, 1) -> Colour.White,
        Intersection (8, 2) -> Colour.White,
        Intersection (8, 3) -> Colour.White,
        Intersection (7, 3) -> Colour.White,
        Intersection (11, 11) -> Colour.White,
        Intersection (11, 12) -> Colour.White,
        Intersection (12, 11) -> Colour.White,
        Intersection (12, 12) -> Colour.White))

      b.groups (Colour.White).size must_== 2
      b.groups (Colour.Black).size must_== 1
    }

    "know the rules of capture" in {
      val bT = Board.createS (19, Map (
        "D:4" -> Colour.White,
        "C:4" -> Colour.Black,
        "E:4" -> Colour.Black,
        "D:3" -> Colour.Black)).applyMove ("D:5", Colour.Black)

      bT match {
        case Success (b) =>
          b.stones.size must_== 4
          b ("D:4") must_== Success (None)
          b ("C:4") must_== Success (Some (Colour.Black))
          b ("E:4") must_== Success (Some (Colour.Black))
          b ("D:3") must_== Success (Some (Colour.Black))
          b ("D:5") must_== Success (Some (Colour.Black))
        case _ => ko
      }
    }
  }
}
