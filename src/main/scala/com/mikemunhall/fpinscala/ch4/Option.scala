package com.mikemunhall.fpinscala.ch4

sealed trait Option[+A] {
  // Exercise 4.1 - 1/5
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  // Exercise 4.1 - 2/5
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  // Exercise 4.1 - 3/5
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  // Exercise 4.1 - 4/5
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  // Exercise 4.1 - 5/5
  def filter(f: A => Boolean): Option[A] = flatMap(v => if(f(v)) Some(v) else None)

}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // Exercise 4.3 - Using a matcher
  def map2m[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  // Exercise 4.3 - Using flatMap and map
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(ai => b.map(bi => f(ai, bi)))

  // Just for fun
  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    a.flatMap(ai => b.flatMap(bi => c.map(ci => f(ai, bi, ci))))

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hi => sequence(t).map(hi :: _))
  }

  // Exercise 4.5 - 1/2 - using map
  def traverseM[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight {
      Some(Nil): Option[List[B]]
    } { (aa, acc) =>
      f(aa).flatMap(aaa => acc.map(aaa :: _))
    }
  }

  // Exercise 4.5 - 1/2 - using map2
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight {
      Some(Nil): Option[List[B]]
    } { (h, t) =>
      map2(f(h), t)(_ :: _)
    }
  }

  // Exercise 4.5 - 2/2
  def sequenceT[A](a: List[Option[A]]): Option[List[A]] = ???

}