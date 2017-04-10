package com.mikemunhall.fpinscala.ch4

sealed trait Either[+E, +A] {

  // TODO next: Write tests for map and flatMap
  // TODO last: Replace pattern matching
  // Exercise 4.6 - 1/4
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  // Exercise 4.6 - 2/4
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  // Exercise 4.6 - 3/4
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }

  // Exercise 4.6 - 4/4 - using matcher
  def map2M[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa, bb)))

  // Exercise 4.6 - 4/4
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Exercise 4.7 - 1/2 - using matcher
  def sequenceM[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => h.flatMap(hh => sequenceM(t).map(hh :: _))
  }

  // Exercise 4.7 - 2/2
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(esa => esa)
}
