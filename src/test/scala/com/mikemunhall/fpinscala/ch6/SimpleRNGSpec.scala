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
      val (result, _) = rng.nonNegativeIntMine(rng)
      (result >= 0) must beTrue
      result === 549383846
    }

    "book" >> {
      val rng = SimpleRNG(25214903928l)
      val (result, _) = rng.nonNegativeInt(rng)
      (result >= 0) must beTrue
      result === 1151252338
    }
  }

  "double" >> {
    val rng = SimpleRNG(25214903928l)
    val (result, _) = rng.double(rng)
    result === 0.5360936457291245
  }

  "intDouble" >> {
    val rng = SimpleRNG(25214903928l)
    val (result, _) = rng.intDouble(rng)
    result._1 === 1151252338
    result._2 === 0.2558267889544368
  }

  "doubleInt" >> {
    val rng = SimpleRNG(25214903928l)
    val (result, _) = rng.doubleInt(rng)
    result._1 === 0.5360936457291245
    result._2 === 549383846
  }

  "double3" >> {
    val rng = SimpleRNG(25214903928l)
    val (result, _) = rng.double3(rng)
    result._1 === 0.5360936457291245
    result._2 === 0.2558267889544368
    result._3 === 0.7510961224325001
  }
}
