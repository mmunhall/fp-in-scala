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
  }
}
