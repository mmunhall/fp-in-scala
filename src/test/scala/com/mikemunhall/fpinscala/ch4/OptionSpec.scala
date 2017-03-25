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
}
