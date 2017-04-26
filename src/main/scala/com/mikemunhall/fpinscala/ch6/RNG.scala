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
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}