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
    "using matcher" >> {
      Stream(1, 2, 3, 4, 5, 6).takeWhileM(_ % 2 == 0) === Empty
      Stream(1, 2, 3, 4, 5, 6).takeWhileM(_ % 2 != 0).toList === List(1)
      Stream(2, 4, 6, 1, 2, 8).takeWhileM(_ % 2 == 0).toList === List(2, 4, 6)
      Stream[Int]().takeWhileM(_ % 2 == 0) === Empty  
    }
    
    "using foldRight" >> {
      Stream(1, 2, 3, 4, 5, 6).takeWhile(_ % 2 == 0) === Empty
      Stream(1, 2, 3, 4, 5, 6).takeWhile(_ % 2 != 0).toList === List(1)
      Stream(2, 4, 6, 1, 2, 8).takeWhile(_ % 2 == 0).toList === List(2, 4, 6)
      Stream[Int]().takeWhile(_ % 2 == 0) === Empty
    }
  }

  "exists implemented with foldRight" >> {
    Stream(10, 9, 8, 7).exists(_ <= 7) === true
    Stream(10, 9, 8, 7).exists(_ <= 1) === false
  }

  "forAll" >> {
    Stream[Int]().forAll(_ % 2 == 0) === true
    Stream(2, 4, 6).forAll(_ % 2 == 0) === true
    Stream(2, 4, 1).forAll(_ % 2 == 0) === false
    Stream(1, 2, 4).forAll(_ % 2 == 0) === false
    Stream(1).forAll(_ % 2 == 0) === false
  }

  "headOption" >> {
    "using matcher" >> {
      Stream[Int]().headOptionM === None
      Stream(1, 2, 3).headOptionM === Some(1)
    }

    "using foldRight" >> {
      Stream[Int]().headOption === None
      Stream(1, 2, 3).headOption === Some(1)
    }
  }

  "map" >> {
    Stream(1, 2, 3).map(_.toDouble).toList === List(1.0, 2.0, 3.0)
    Stream[Double]().map(_.toString).toList === List[String]()
    Stream[Double]().map(_.toString) === Empty
  }

  "filter" >> {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList === List(2, 4)
    Stream(1, 3, 5).filter(_ % 2 == 0) === Empty
    Stream[Int]().filter(_ % 2 == 0) === Empty
  }

  "append" >> {
    Stream(1, 2).append(Stream(3)).toList === List(1, 2, 3)
    Stream().append(Stream(3)).toList === List(3)
  }

  "flatMap" >> {
    Stream(1, 2, 3).flatMap(a => Stream(a - 1, a, a + 1)).toList === List(0, 1, 2, 1, 2, 3, 2, 3, 4)
  }

  "infinite stream" >> {
    val ones: Stream[Int] = Stream.cons(1, ones)
    ones.take(5).toList === List(1, 1, 1, 1, 1)
  }
}
