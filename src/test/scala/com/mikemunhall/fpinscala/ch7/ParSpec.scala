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

  "choice" >> {
    "less general version" >> {
      val par1 = Par.unit(1)
      val par2 = Par.unit(2)
      val es = Executors.newFixedThreadPool(2)
      Par._choice(Par.unit(true))(par1, par2)(es).get === 1
      Par._choice(Par.unit(false))(par1, par2)(es).get === 2
    }
    /*"more general version" >> {
      val par1 = Par.unit(1)
      val par2 = Par.unit(2)
      val es = Executors.newFixedThreadPool(2)
      Par.choice(Par.unit(true))(par1, par2)(es).get === 1
      Par.choice(Par.unit(false))(par1, par2)(es).get === 2
    }*/
  }

  "choiceN" >> {
    "less general version" >> {
      val par0 = Par.unit(0)
      val par1 = Par.unit(1)
      val par2 = Par.unit(2)
      val es = Executors.newFixedThreadPool(2)
      Par._choiceN(par0)(List(par0, par1, par2))(es).get === 0
      Par._choiceN(par1)(List(par0, par1, par2))(es).get === 1
      Par._choiceN(par2)(List(par0, par1, par2))(es).get === 2
    }
    "more general version" >> {
      val par0 = Par.unit(0)
      val par1 = Par.unit(1)
      val par2 = Par.unit(2)
      val es = Executors.newFixedThreadPool(2)
      Par.choiceN(par0)(List(par0, par1, par2))(es).get === 0
      Par.choiceN(par1)(List(par0, par1, par2))(es).get === 1
      Par.choiceN(par2)(List(par0, par1, par2))(es).get === 2
    }
  }

  "choiceMap" >> {
    val parK = Par.unit(22)
    val choices = Map(0 -> Par.unit("No"), 22 -> Par.unit("Yes"))
    val es = Executors.newFixedThreadPool(2)
    Par.choiceMap(parK)(choices)(es).get === "Yes"
  }

  "chooser aka flatMap" >> {
    val parK = Par.unit(22)
    def choices(a: Int) = a match {
      case n if n <= 0 => Par.unit("LTE 0")
      case _ => Par.unit("GT 0")
    }
    val es = Executors.newFixedThreadPool(2)
    Par.flatMap(parK)(choices)(es).get === "GT 0"
  }
}
