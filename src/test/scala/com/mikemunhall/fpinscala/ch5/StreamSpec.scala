package com.mikemunhall.fpinscala.ch5

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "toList" >> {
    "non-stack safe version" >> {
      Stream(1, 2, 3).toListRecursive === List(1, 2, 3)
      Stream().toListRecursive === Nil
    }

    "stack-safe version" >> {
      Stream(1, 2, 3).toList === List(1, 2, 3)
      Stream().toList === Nil
    }
  }
}
