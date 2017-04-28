package com.mikemunhall.fpinscala.ch6

trait RNG {
  def nextInt: (Int, RNG)

  // Exercise 1 - Mine
  def nonNegativeIntMine(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (r, nextRng) if r > 0 || r == Int.MinValue => (r, nextRng)
    case (r, nextRng) => nonNegativeInt(nextRng)
  }

  // Exercise 1 - Book
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 3 - 1/3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  // Exercise 3 - 2/3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = nonNegativeInt(rng1)
    ((d, i), rng2)
  }

  // Exercise 3 - 3/3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // Exercise 4
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
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}