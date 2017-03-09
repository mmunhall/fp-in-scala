package com.mikemunhall.fpinscala.ch2

import org.specs2.mutable.Specification

class MainSpec extends Specification {

  "fib" >> {
    "return the nth fibonnacci number" >> {
      Main.fib(1) === 0
      Main.fib(2) === 1
      Main.fib(3) === 1
      Main.fib(4) === 2
      Main.fib(5) === 3
      Main.fib(6) === 5
      Main.fib(7) === 8
      Main.fib(8) === 13
      Main.fib(9) === 21
      Main.fib(10) === 34
    }
  }

  "isSorted" >> {
    "is true when items are ordered" >> {
      Main.isSorted[Int](Array(1, 2, 3, 4), (x, y) => x <= y) must beTrue
      Main.isSorted[Int](Array(9), (x, y) => x <= y) must beTrue
      Main.isSorted[Int](Array(4, 1, 2, 3), (x, y) => x <= y) must beFalse
      Main.isSorted[Int](Array(1, 2, 4, 3), (x, y) => x <= y) must beFalse
      Main.isSorted[Char](Array('a', 'b', 'c', 'd'), (a, b) => a <= b) must beTrue
      Main.isSorted[Char](Array('z', 'b', 'c', 'd'), (a, b) => a <= b) must beFalse
      Main.isSorted[Char](Array('.'), (a, b) => a <= b) must beTrue
      Main.isSorted[String](Array("A", "a"), (a, b) => a.compareTo(b) <= 0) must beTrue
      Main.isSorted[String](Array("a", "A"), (a, b) => a.compareTo(b) <= 0) must beFalse
      Main.isSorted[String](Array("hi", "mom"), (a, b) => a.compareToIgnoreCase(b) <= 0) must beTrue
      Main.isSorted[String](Array("mom", "hi"), (a, b) => a.compareToIgnoreCase(b) <= 0) must beFalse
      Main.isSorted[String](Array("hi", "mom"), (a, b) => a.compareToIgnoreCase(b) <= 0) must beTrue
      Main.isSorted[String](Array("mom", "hi"), (a, b) => a.compareToIgnoreCase(b) <= 0) must beFalse
    }
  }
}
