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
    "recursively" >> {
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
    "using unfold" >> {
      Stream(1, 2, 3, 4, 5).takeU(3).toList === List(1, 2, 3)
      Stream(1, 2, 3, 4, 5).takeU(20).toList === List(1, 2, 3, 4, 5)
      Stream(1, 2, 3, 4, 5).takeU(0) === Empty
      Stream().takeU(5) === Empty
    }
  }

  "drop" >> {
    Stream(1, 2, 3, 4, 5).drop(3).toList === List(4, 5)
    Stream(1, 2, 3, 4, 5).drop(20) === Empty
    Stream(1, 2, 3, 4, 5).drop(0).toList === List(1, 2, 3, 4, 5)
    Stream().drop(5) === Empty
  }

  "takeWhile" >> {
    "recursively" >> {
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
    "using unfold" >> {
      Stream(1, 2, 3, 4, 5, 6).takeWhileU(_ % 2 == 0) === Empty
      Stream(1, 2, 3, 4, 5, 6).takeWhileU(_ % 2 != 0).toList === List(1)
      Stream(2, 4, 6, 1, 2, 8).takeWhileU(_ % 2 == 0).toList === List(2, 4, 6)
      Stream[Int]().takeWhileU(_ % 2 == 0) === Empty
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
    "using foldRight" >> {
      Stream(1, 2, 3).map(_.toDouble).toList === List(1.0, 2.0, 3.0)
      Stream[Double]().map(_.toString).toList === List[String]()
      Stream[Double]().map(_.toString) === Empty 
    }
    "using unfold" >> {
      Stream(1, 2, 3).mapU(_.toDouble).toList === List(1.0, 2.0, 3.0)
      Stream[Double]().mapU(_.toString).toList === List[String]()
      Stream[Double]().mapU(_.toString) === Empty
    }
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
    "ones" >> {
      type StreamType = Cons[_]
      "recursive" >> {
        Stream.ones.take(5).toList === List(1, 1, 1, 1, 1)
        Stream.ones.map(_ + 1).exists(_ % 2 == 0) must beTrue
        Stream.ones.takeWhile(_ == 1) must beAnInstanceOf[StreamType]
        Stream.ones.forAll(_ != 1) must beFalse
      }
      "using constantU" >> {
        Stream.onesU.take(5).toList === List(1, 1, 1, 1, 1)
        Stream.onesU.map(_ + 1).exists(_ % 2 == 0) must beTrue
        Stream.onesU.takeWhile(_ == 1) must beAnInstanceOf[StreamType]
        Stream.onesU.forAll(_ != 1) must beFalse
      }
    }

    "using constant" >> {
      "recursive" >> {
        val twos = Stream.constant(2)
        twos.take(5).toList === List(2, 2, 2, 2, 2)
        twos.map(_ + 2).exists(_ % 2 == 0) must beTrue
      }
      "using unfold" >> {
        Stream.constantU("a").take(5).toList === List("a", "a", "a", "a", "a")
      }
    }

    "from" >> {
      "recursive" >> {
        Stream.from(0).take(10).toList === List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      }
      "using unfold" >> {
        Stream.fromU(0).take(10).toList === List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      }
    }

    "fibs" >> {
      "recursive" >> {
        Stream.fibs.take(7).toList === List(0, 1, 1, 2, 3, 5, 8)
      }
      "using unfold" >> {
        Stream.fibsU.take(7).toList === List(0, 1, 1, 2, 3, 5, 8)
      }
    }

    "unfold" >> {
      type StreamType = Cons[_]
      Stream.unfold(0)(a => if (a <= 10) Some((a, a + 2)) else None).toList === List(0, 2, 4, 6, 8, 10)
      Stream.unfold(0)(a => Some((a, a + 2))).take(3).toList === List(0, 2, 4)
    }

    "zipWith" >> {
      Stream(1, 2, 3).zipWith(Stream(.5, 1.0, 2.0))((a, b) => (a * b).toString).toList === List("0.5", "2.0", "6.0")
      Stream(1, 2).zipWith(Stream(.5, 1.0, 2.0))((a, b) => (a * b).toString).toList === List("0.5", "2.0")
      Stream(1, 2, 3).zipWith(Stream(.5, 1.0))((a, b) => (a * b).toString).toList === List("0.5", "2.0")
    }

    "zipAll" >> {
      Stream(1, 2, 3).zipAll(Stream('a', 'b', 'c')).toList ===
        List((Some(1), Some('a')), (Some(2), Some('b')), (Some(3), Some('c')))
      Stream(1, 2, 3).zipAll(Stream('a', 'b')).toList ===
        List((Some(1), Some('a')), (Some(2), Some('b')), (Some(3), None))
      Stream(1, 2).zipAll(Stream('a', 'b', 'c')).toList ===
        List((Some(1), Some('a')), (Some(2), Some('b')), (None, Some('c')))
    }

    "startsWith" >> {
      "mine" >> {
        Stream(1, 2, 3).startsWithMine(Stream(1, 2)) === true
        Stream(1, 2, 3).startsWithMine(Stream(2)) === false
      }
      "companion book" >> {
        Stream(1, 2, 3).startsWith(Stream(1, 2)) === true
        Stream(1, 2, 3).startsWith(Stream(2)) === false
      }
    }

    "tails" >> {
      "mine" >> {
        Stream(1, 2, 3).tailsMine.toList.map(_ toList) === List(List(1, 2, 3), List(2, 3), List(3), List())
      }
      "theirs" >> {
        Stream(1, 2, 3).tails.toList.map(_ toList) === List(List(1, 2, 3), List(2, 3), List(3), List())
      }
    }
  }

}
