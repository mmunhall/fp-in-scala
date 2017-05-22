package com.mikemunhall.fpinscala.ch7

import java.util.concurrent.Executors

import org.specs2.mutable.Specification

class ParSpec extends Specification {

  "parFilter" >> {
    val par1 = Par.parFilter(List(1, 2, 3, 4))(_ >= 2)
    val par2 = Par.parFilter(List(1, 2, 3, 4))(_ % 2 == 0)
    Par.run(Executors.newFixedThreadPool(2))(par1).get === List(2, 3, 4)
    Par.run(Executors.newFixedThreadPool(2))(par2).get === List(2, 4)
  }

  "equals" >> {
    val par1 = Par.unit(1)
    val par2 = Par.unit(2)
    val par3 = Par.unit(1)

    Par.equals(Executors.newFixedThreadPool(2))(par1, par2) === false
    Par.equals(Executors.newFixedThreadPool(2))(par1, par3) === true
  }

  // Begin Exercise 7.9
  "fork deadlocks with one thread" >> {
    skipped("Creates a deadlock")
    val a = Par.lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    Par.equals(S)(a, Par.fork(a)) === true
  }

  "deadlock averted with two threads" >> {
    val a = Par.lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(2)
    Par.equals(S)(a, Par.fork(a)) === true
  }

  "fork deadlocks with two threads" >> {
    skipped("Creates a deadlock")
    val a = Par.lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    Par.equals(S)(a, Par.fork(Par.fork(a))) === true
  }
  // End Exercise 7.9

}
