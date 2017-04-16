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

  "take" >> {
    "takeR (reversed)" >> {
      Stream(1, 2, 3, 4, 5).takeR(3).toList.reverse === List(1, 2, 3)
      Stream(1, 2, 3, 4, 5).takeR(20).toList.reverse === List(1, 2, 3, 4, 5)
      Stream(1, 2, 3, 4, 5).takeR(0) === Empty
      Stream().takeR(5) === Empty
    }

    "take" >> {
      Stream(1, 2, 3, 4, 5).take(3).toList === List(1, 2, 3)
      Stream(1, 2, 3, 4, 5).take(20).toList === List(1, 2, 3, 4, 5)
      Stream(1, 2, 3, 4, 5).take(0) === Empty
      Stream().take(5) === Empty
    }
  }

  "drop" >> {
    Stream(1, 2, 3, 4, 5).drop(3).toList === List(4, 5)
    Stream(1, 2, 3, 4, 5).drop(20) === Empty
    Stream(1, 2, 3, 4, 5).drop(0).toList === List(1, 2, 3, 4, 5)
    Stream().drop(5) === Empty
  }

  "takeWhile" >> {
    Stream(1, 2, 3, 4, 5, 6).takeWhile(_ % 2 == 0) === Empty
    Stream(1, 2, 3, 4, 5, 6).takeWhile(_ % 2 != 0).toList === List(1)
    Stream(2, 4, 6, 1, 2, 8).takeWhile(_ % 2 == 0).toList === List(2, 4, 6)
    Stream[Int]().takeWhile(_ % 2 == 0) === Empty
  }
}
