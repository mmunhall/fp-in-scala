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

  // Exercise 6.4 - Recursively
  def intsR(count: Int)(rng: RNG): (List[Int], RNG) = {

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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, _) = s(rng)
      (f(a), rng)
    }
  }

  // Exercise 6.5
  val double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (rav, rar) = ra(rng)
      val (rbv, rbr) = rb(rar)
      (f(rav, rbv), rbr)
    }
  }

  def map4[A, B, C, D, E](ra: Rand[A], rb: Rand[B], rc: Rand[C], rd: Rand[D])(f: (A, B, C, D) => E): Rand[E] = {
    rng => {
      val (rav, rar) = ra(rng)
      val (rbv, rbr) = rb(rng)
      val (rcv, rcr) = rc(rng)
      val (rdv, rdr) = rd(rng)
      (f(rav, rbv, rcv, rdv), rng)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val int: Rand[Int] = _.nextInt

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 6.7 - 1/2
  // def sequence[A](fs: List[RNG => (A, RNG)]): RNG => (List[A], RNG)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) { (a, acc) =>
      map2(a, acc)(_ :: _)
    }

  // Exercise 6.7 - 2/2
  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThanR(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2) else nonNegativeLessThan(n)(rng)
  }

  // Exercise 6.8 - 1/2
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (i, rng2) = f(rng)
    g(i)(rng2)
  }

  // Exercise 6.8 2/2
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { a =>
    val mod = a % n
    if (a + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
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