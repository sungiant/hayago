package hayago.game.tests

import org.specs2.mutable._
import hayago.game._

class GtpSpec extends Specification { sequential
  import hayago._

  "The Gtp module should" should {
    "convert gtp vertex strings to Hayago intersections correctly" in {
      gtp.toIntersection ("A1") must_== Some (Intersection (0, 0))
      gtp.toIntersection ("C5") must_== Some (Intersection (2, 4))
      gtp.toIntersection ("B10") must_== Some (Intersection (1, 9))
      gtp.toIntersection ("b15") must_== Some (Intersection (1, 14))
      gtp.toIntersection ("F11") must_== Some (Intersection (5, 10))
      gtp.toIntersection ("-") must_== None
      gtp.toIntersection ("?") must_== None
      gtp.toIntersection ("1-1") must_== None
      gtp.toIntersection ("a 1") must_== None
      gtp.toIntersection ("B-14") must_== None
    }
    "convert Hayago intersections to gtp vertex strings correctly" in {
      gtp.toGtpString (Intersection (0, 0)) must_== "A1"
      gtp.toGtpString (Intersection (2, 4)) must_== "C5"
      gtp.toGtpString (Intersection (1, 9)) must_== "B10"
      gtp.toGtpString (Intersection (1, 14)) must_== "B15"
      gtp.toGtpString (Intersection (5, 10)) must_== "F11"
    }
  }
}
