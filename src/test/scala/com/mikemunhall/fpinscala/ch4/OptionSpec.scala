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
}
