package com.mikemunhall.fpinscala.ch8

import com.mikemunhall.fpinscala.ch6.{RNG, State}
import com.mikemunhall.fpinscala.ch5.Stream
import com.mikemunhall.fpinscala.ch8.Gen.{FailedCase, SuccessCount, TestCases}

case class Prop(run: (TestCases, RNG) => Result) {

  // Exercise 8.9 - 1/2
  def &&(p: Prop): Prop = Prop {
    (n, rng) => run(n, rng) match {
      case Passed => p.run(n, rng)
      case x => x
    }
  }

  // Exercise 8.9 - 2/2
  def ||(p: Prop): Prop = Prop {
    (n, rng) => run(n, rng) match {
      case Falsified(_, _) => p.run(n, rng)
      case x => x
    }
  }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Gen {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  // Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
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

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(if (_) g1 else g2)
  }

  // Exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???
}

case class Gen[A](sample: State[RNG, A]) {

  // Exercise 8.6 - 1/2
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  // Exercise 8.6 - 2/2
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => Gen.listOfN(n, this))

}