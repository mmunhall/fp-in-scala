package com.mikemunhall.fpinscala.ch7

import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.concurrent.duration.TimeUnit

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](par1: Par[A], par2: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = par1(es)
      val bf = par2(es)
      UnitFuture(f(af.get, bf.get))
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call() = a(es).get
    })

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

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

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def isCancelled = false
    def get(timeout: Long, unit: TimeUnit) = get
    def cancel(myInterruptIfRunning: Boolean) = false
  }

}