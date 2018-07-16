package com.mikemunhall.fpinscala

object RecursiveFunctions {

  // Exercise 2.1
  // Fibonacci: [0, 1, 1, 2, 3, 5, 8...]
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(m: Int, n1: Int, acc: Int): Int = {
      if (m == n) n1 + acc
      else go(m + 1, if (m == 1) 1 else acc, n1 + acc)
    }

    go(1, 0, 0): Int
  }
}