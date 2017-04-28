package com.mikemunhall.fpinscala.ch6

import org.specs2.mutable.Specification

class SimpleRNGSpec extends Specification {

  "RNG is referentially transparent and deterministic (but, therefore, not very \"random.\")" >> {
    val (number1, nextRng1) = SimpleRNG(1).nextInt
    number1 === 384748
    nextRng1 === SimpleRNG(25214903928l)
    val (number2, nextRng2) = nextRng1.nextInt
    number2 === -1151252339
    nextRng2 === SimpleRNG(206026503483683l)
  }

  "nonNegativeInt" >> {
    "mine" >> {
      val rng = SimpleRNG(25214903928l)
      val (result, _) = RNG.nonNegativeIntMine(rng)
      (result >= 0) must beTrue
      result === 549383846
    }

    "book" >> {
      val rng = SimpleRNG(25214903928l)
      val (result, _) = RNG.nonNegativeInt(rng)
      (result >= 0) must beTrue
      result === 1151252338
    }
  }

  "nonNegativeEven" >> {
    val rng = SimpleRNG(25214903928l)
    val (result, _) = RNG.nonNegativeEven(rng)
    result === 1151252338
  }

  "double" >> {
    "using original method" >> {
      val rng = SimpleRNG(25214903928l)
      val (result, _) = RNG.double1(rng)
      result === 0.5360936457291245
    }
    "using map" >> {
      val rng = SimpleRNG(25214903928l)
      val (result, _) = RNG.double(rng)
      result === 0.5360936457291245
    }
  }

  "intDouble" >> {
    val rng = SimpleRNG(25214903928l)
    val (result, _) = RNG.intDouble(rng)
    result._1 === 1151252338
    result._2 === 0.2558267889544368
  }

  "doubleInt" >> {
    val rng = SimpleRNG(25214903928l)
    val (result, _) = RNG.doubleInt(rng)
    result._1 === 0.5360936457291245
    result._2 === 549383846
  }

  "double3" >> {
    val rng = SimpleRNG(25214903928l)
    val (result, _) = RNG.double3(rng)
    result._1 === 0.5360936457291245
    result._2 === 0.2558267889544368
    result._3 === 0.7510961224325001
  }

  "ints" >> {
    val rng = SimpleRNG(25214903928l)
    RNG.ints(0)(rng)._1 === Nil
    RNG.ints(1)(rng)._1 === List(-1151252339)
    RNG.ints(2)(rng)._1 === List(-549383847, -1151252339)
    RNG.ints(3)(rng)._1 === List(1612966641, -549383847, -1151252339)
  }

  "map2" >> {
    val rng = SimpleRNG(25214903928l)
    RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)(_ * _)(rng)._1 === -2077479228
  }
}
