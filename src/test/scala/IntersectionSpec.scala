package hayago.game.tests

import org.specs2.mutable._
import hayago.game._

class IntersectionSpec extends Specification { sequential

  "An Intersection" should {
    "unapply from strings" in {
      Intersection.unapply ("A:1") must_== Some (Intersection (0, 0))
      Intersection.unapply ("C:5") must_== Some (Intersection (2, 4))
      Intersection.unapply ("B:10") must_== Some (Intersection (1, 9))
      Intersection.unapply ("B:15") must_== Some (Intersection (1, 14))
      Intersection.unapply ("F:11") must_== Some (Intersection (5, 10))
      Intersection.unapply ("-") must_== None
      Intersection.unapply ("b:15") must_== None
      Intersection.unapply ("?") must_== None
      Intersection.unapply ("1-1") must_== None
      Intersection.unapply ("a 1") must_== None
      Intersection.unapply ("B14") must_== None
    }
    "stringify correctly" in {
      Intersection (0, 0).toString must_== "A:1"
      Intersection (2, 4).toString must_== "C:5"
      Intersection (1, 9).toString must_== "B:10"
      Intersection (1, 14).toString must_== "B:15"
      Intersection (5, 10).toString must_== "F:11"
    }
  }
}
