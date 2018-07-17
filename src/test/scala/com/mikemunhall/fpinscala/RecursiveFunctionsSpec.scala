package com.mikemunhall.fpinscala

import org.specs2.mutable.Specification

object RecursiveFunctionsSpec extends Specification {

  "fib" >> {
    "return the nth fibonnacci number" >> {
      import RecursiveFunctions.fib
      fib(1) === 0
      fib(2) === 1
      fib(3) === 1
      fib(4) === 2
      fib(5) === 3
      fib(6) === 5
      fib(7) === 8
      fib(8) === 13
      fib(9) === 21
      fib(10) === 34
    }
  }

  "isSorted" >> {
    "reterns true if elements are sorted" >> {
      import RecursiveFunctions.isSorted

      isSorted[Int](Array(1, 2, 3, 4, 5), _ <= _)
      isSorted[Int](Array(1), _ <= _)
      isSorted[Int](Array(), _ <= _)
      isSorted[String](Array("a", "b", "c"), _.compare(_) <= 0)
      isSorted[String](Array("aa", "ab", "ac"), _.compare(_) <= 0)
      isSorted[Int](Array(1, 1, 1), _ <= _)
      isSorted[Char](Array('a', 'a', 'a'), _ <= _)
    }
  }

  "isSorted" >> {
    "returns false if elements are sorted" >> {
      import RecursiveFunctions.isSorted

      isSorted[Int](Array(1, 2, 3, 4, 1), _ < _) === false
      isSorted[Int](Array(3, 2, 3, 4, 5), _ < _) === false
      isSorted[String](Array("c", "b", "c"), _.compare(_) < 0) === false
      isSorted[String](Array("ac", "ab", "ac"), _.compare(_) < 0) === false
    }
  }

}
