package com.mikemunhall.fpinscala.ch2

object Main {

  def fib(n: Int): Int = {

    @annotation.tailrec
    def go(position: Int, l1: Int, l2: Int): Int = {
      if (position == 0) l1
      else go(position - 1, l1 + l2, l1)
    }

    if (n < 1) throw new IllegalArgumentException("n must be a natural number.")
    else if (n == 1) 0
    else if (n == 2) 1
    else go(n - 2, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.size - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}
