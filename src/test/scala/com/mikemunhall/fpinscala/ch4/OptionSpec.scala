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
}
