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

}
