package com.mikemunhall.fpinscala.ch3

import org.specs2.mutable.Specification

class ListSpec extends Specification {

  "tail" >> {
    "return the tail of a list" >> {
      List.tail(List("a", "b", "c")) === List("b", "c")
      List.tail(List("a", "b", "c")) === Cons("b", Cons("c", Nil))
      List.tail(Cons("1", Nil)) === Nil
    }

    "throw an exception when invoked with an empty list" >> {
      List.tail(Nil) must throwA("Empty list")
    }

    "setHead" >> {
      "replace the head of a list with the new head" >> {
        List.setHead(List("a", "b", "c"), "d") === List("d", "b", "c")
        List.setHead(Cons("1", Nil), "2") === List("2")
      }

      "throw an exception when invoked with an empty list" >> {
        List.setHead(Nil, "foo") must throwA("Empty list")
      }
    }

    "drop" >> {
      "removes the first n items from a list" >> {
        List.drop(List(1, 2, 3), 2) === List(3)
        List.drop(List(1, 2, 3), 3) === Nil
      }

      "returns Nil if more items are dropped than are contained in the list" >> {
        List.drop(Nil, 1) === Nil
        List.drop(List(1, 2), 3) === Nil
      }
    }

    "dropWhile" >> {
      "remove heads until the predicate is not satisfied" >> {
        List.dropWhile[Int](List(1, 2, 3), a => a < 3) === List(3)
      }

      "return Nil on an empty list" >> {
        List.dropWhile[Any](Nil, _ => false) === Nil
      }
    }

    "init" >> {
      "remove the last item in a list" >> {
        List.init(List("a", "b")) === List("a")
        List.init(List("a", "b", "c")) === List("a", "b")
        List.init(List("a")) === Nil
      }

      "throw an exception when invoked with an empty list" >> {
        List.init(Nil) must throwA("Empty list")
      }
    }

    "foldRight" >> {
      "fold all elements right to left" >> {
        List.foldRight(List(1, 2, 3), 0)(_ + _) === 6
        List.foldRight(List(1, 2, 3), 1)(_ * _) === 6
      }

      // Exercise 3.8: This is kinda functionally equivalent to apply()
      "reconstructor?" >> {
        List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) === List(1, 2, 3)
      }
    }

    "length" >> {
      "calculates the length of a list" >> {
        List.length(Nil) === 0
        List.length(List(1)) === 1
        List.length(List(1, 2)) === 2
        List.length(List(1, 2, 3)) === 3
      }
    }

    "foldLeft" >> {
      "fold all elements left to right" >> {
        List.foldLeft(List(1, 2, 3), 0)(_ + _) === 6
        List.foldLeft(List("one", "two", "three"), 0)(_ + _.length) === 11
      }
    }

    "sumL" >> {
      "sums ints in a list using foldLeft" >> {
        List.sumL(List(3, 4, 5)) === 12
      }
    }

    "productL" >> {
      "return product of doubles in a list using foldLeft" >> {
        List.productL(List(3, 4, 5)) === 60
      }
    }

    "lengthL" >> {
      "return length of list using foldLeft" >> {
        List.lengthL(List(3, 4, 5)) === 3
      }
    }
  }
}
