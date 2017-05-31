package com.mikemunhall.fpinscala.ch7

import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.concurrent.duration.TimeUnit

object Par {

  type Par[A] = ExecutorService => Future[A]

  /**
    * promotes a constant value to a parallel computation.
    */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /**
    * combines the results of two parallel computations with a binary function.
    */
  def map2[A, B, C](par1: Par[A], par2: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = par1(es)
      val bf = par2(es)
      UnitFuture(f(af.get, bf.get))
    }

  /**
    * marks a computation for concurrent evaluation. The evaluation won’t actually occur until forced by run.
    */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call() = a(es).get
    })

  /**
    * wraps its unevaluated argument in a Par and marks it for concurrent evaluation.
    */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
    * extracts a value from a Par by actually performing the computation.
    */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // Exercise 7.4
  /**
    * convert any function A => B to one that evaluates its result asynchronously
    */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](par: Par[A])(f: A => B): Par[B] = map2(par, unit(()))((a, _) => f(a))

  def parSort(par: Par[List[Int]]): Par[List[Int]] = map(par)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))
  }

  // Exercise 7.6 (This may not be right.)
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldRight(unit(List[A]()))((a, acc) => if (f(a)) map2(unit(a), acc)(_ :: _) else acc)

  def equals[A](es: ExecutorService)(par1: Par[A], par2: Par[A]): Boolean =
    par1(es).get == par2(es).get

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // Exercsise 7.11 - 1/2
  def _choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => run(es)(choices(run(es)(n).get))

  // Exercise 7.11 - 2/2
  def _choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    _choiceN(map(cond)(c => if (c) 0 else 1))(List(t, f))
  }

  // Exercise 7.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => run(es)(choices(run(es)(key).get))

  // Exercise 7.13 - 1/3
  // def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => run(es)(choices(run(es)(pa).get))

  // Exercise 7.13 - 2/3
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(choices(_))

  // Exercise 7.13 - 3/3
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(if (_) t else f)

  // Exercise 7.14 - 1/3
  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

  // Exercise 7.14 - 2/3
  def flatMapViaJoin[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))

  // Exercise 7.14 - 3/3
  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(a => a)

  private case class UnitFuture[A](a: A) extends Future[A] {
    def isDone = true
    def isCancelled = false
    def get = a
    def get(timeout: Long, unit: TimeUnit) = get
    def cancel(myInterruptIfRunning: Boolean) = false
  }
}

