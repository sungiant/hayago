package hayago.game.tests

import org.specs2.mutable._
import hayago.game._

class SgfSpec extends Specification { sequential
  import hayago._

  "The Sgf module should" should {
    "convert sgf intersection strings to Hayago intersections correctly" in {
      sgf.toIntersection ("aa") must_== Some (Intersection (0, 0))
      sgf.toIntersection ("ce") must_== Some (Intersection (2, 4))
      sgf.toIntersection ("bj") must_== Some (Intersection (1, 9))
      sgf.toIntersection ("bo") must_== Some (Intersection (1, 14))
      sgf.toIntersection ("fk") must_== Some (Intersection (5, 10))
      sgf.toIntersection ("-") must_== None
      sgf.toIntersection ("?") must_== None
      sgf.toIntersection ("1-1") must_== None
      sgf.toIntersection ("a1") must_== None
      sgf.toIntersection ("B14") must_== None
    }
    "convert Hayago intersections to sgf intersection strings corrextly" in {
      sgf.toSgfString (Intersection (0, 0)) must_== "aa"
      sgf.toSgfString (Intersection (2, 4)) must_== "ce"
      sgf.toSgfString (Intersection (1, 9)) must_== "bj"
      sgf.toSgfString (Intersection (1, 14)) must_== "bo"
      sgf.toSgfString (Intersection (5, 10)) must_== "fk"
    }
  }
}
