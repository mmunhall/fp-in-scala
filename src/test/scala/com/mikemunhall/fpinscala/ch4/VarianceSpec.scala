package com.mikemunhall.fpinscala.ch4

import org.specs2.mutable.Specification
import Variance._

class VarianceSpec extends Specification {

  "mean" >> {
    mean(Nil) === None
    mean(List(2.0, 4.0)) === Some(3.0)
  }

  "variance" >> {
    variance(Nil) === None
    variance(List(2.0, 4.0)) === Some(1.0)
    variance(List(21.3, 966.432, -.112)) === Some(203104.47501155557)
    variance(List(9.0)) === Some(0.0)
  }
}
