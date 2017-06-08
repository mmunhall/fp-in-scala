package com.mikemunhall.fpinscala.ch8

import com.mikemunhall.fpinscala.ch6.{RNG, State}
import com.mikemunhall.fpinscala.ch8.Prop.{FailedCase, SuccessCount}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  /*def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }*/
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

object Gen {
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  // Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  // Exercise 8.5 - 1/3
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  // Exercise 8.5 - 2/3
  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeInt).map(n => if (n % 2 == 0) true else false))
  }

  // Exercise 8.5 - 3/3
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }
}

case class Gen[A](sample: State[RNG, A])