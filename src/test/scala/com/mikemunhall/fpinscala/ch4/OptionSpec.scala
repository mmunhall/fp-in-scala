package com.mikemunhall.fpinscala.ch4

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "map" >> {
    Some(1).map(_.toString) === Some("1")
    None.map(_.toString) === None
  }

  "flatMap" >> {
    Some(1).flatMap(v => Some(v.toString)) === Some("1")
    None.flatMap(v => Some(v.toString)) === None
  }

  "getOrElse" >> {
    Some(1).getOrElse(2) === 1
    None.getOrElse(2) === 2
  }

  "orElse" >> {
    Some(1).orElse(Some(2)) === Some(1)
    None.orElse(Some(2)) === Some(2)
  }

  "filter" >> {
    Some(1).filter(_ < 10) === Some(1)
    Some(1).filter(_ < 1) === None
    None.filter((a: Int) => a < 1) === None
  }

  "lift" >> {
    def liftedAbs = Option.lift(math.abs)

    liftedAbs(Some(-1)) === Some(1)
    liftedAbs(None) === None
  }

  "map2" >> {
    "using the matcher version" >> {
      Option.map2m[Int, Int, Int](None, Some(1))(_ + _) === None
      Option.map2m[Int, Int, Int](Some(1), None)(_ + _) === None
      Option.map2m[Int, Int, Int](None, None)(_ + _) === None
      Option.map2m[Int, Int, Int](Some(2), Some(1))(_ + _) === Some(3)
    }

    "using flatMap" >> {
      Option.map2[Int, Int, Int](None, Some(1))(_ + _) === None
      Option.map2[Int, Int, Int](Some(1), None)(_ + _) === None
      Option.map2[Int, Int, Int](None, None)(_ + _) === None
      Option.map2[Int, Int, Int](Some(2), Some(1))(_ + _) === Some(3)
    }
  }

  "map3" >> {
    Option.map3[Int, Int, Int, Int](None, Some(1), Some(2))(_ + _ + _) === None
    Option.map3[Int, Int, Int, Int](Some(1), None, Some(2))(_ + _ + _) === None
    Option.map3[Int, Int, Int, Int](None, None, None)(_ + _ + _) === None
    Option.map3[Int, Int, Int, Int](Some(2), Some(1), Some(3))(_ + _ + _) === Some(6)
  }

  "sequence" >> {
    "using map" >> {
      Option.sequenceM(List(Some(1), Some(2))) === Some(List(1, 2))
      Option.sequenceM(List(Some(1), None)) === None
    }

    "using traverse" >> {
      Option.sequence(List(Some(1), Some(2))) === Some(List(1, 2))
      Option.sequence(List(Some(1), None)) === None
    }
  }

  "traverse" >> {
    "using map" >> {
      Option.traverseM(List(1, 2, 3))(Some(_)) === Some(List(1, 2, 3))
      Option.traverseM(List(2, 4, 6))(a => if (a % 2 == 0) Some(1) else None) === Some(List(1, 1, 1))
      Option.traverseM(List(1, 2, 3))(a => if (a % 2 == 0) Some(1) else None) === None
    }

    "using map2" >> {
      Option.traverse(List(1, 2, 3))(Some(_)) === Some(List(1, 2, 3))
      Option.traverse(List(2, 4, 6))(a => if (a % 2 == 0) Some(1) else None) === Some(List(1, 1, 1))
      Option.traverse(List(1, 2, 3))(a => if (a % 2 == 0) Some(1) else None) === None
    }
  }
}
