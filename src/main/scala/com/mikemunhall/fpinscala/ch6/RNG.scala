package com.mikemunhall.fpinscala.ch6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // Exercise 6.1 - Mine
  def nonNegativeIntMine(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (r, nextRng) if r > 0 || r == Int.MinValue => (r, nextRng)
    case (r, nextRng) => nonNegativeInt(nextRng)
  }

  // Exercise 6.1 - Book
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 6.2
  def double1(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 6.3 - 1/3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double1(rng1)
    ((i, d), rng2)
  }

  // Exercise 6.3 - 2/3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double1(rng)
    val (i, rng2) = nonNegativeInt(rng1)
    ((d, i), rng2)
  }

  // Exercise 6.3 - 3/3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double1(rng)
    val (d2, rng2) = double1(rng1)
    val (d3, rng3) = double1(rng2)
    ((d1, d2, d3), rng3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @annotation.tailrec
    def go(c: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (c == 0) (acc, rng)
      else {
        val (n, rngNext) = rng.nextInt
        go(c - 1, n :: acc, rngNext)
      }
    }

    go(count, Nil, rng)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // type Rand[+A] = RNG => (A, RNG)

  // def map[A, B](s: Rand[B])(f: A => B): RNG => (B, RNG) = {
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, _) = s(rng)
      (f(a), rng)
    }
  }

  // Exercise 6.5
  def double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (rav, rar) = ra(rng)
      val (rbv, rbr) = rb(rng)
      (f(rav, rbv), rng)
    }
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}