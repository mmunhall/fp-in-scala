package com.mikemunhall.fpinscala.ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new Exception("Empty list")
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => throw new Exception("Empty list")
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (l, n) if n == 0 => l
    case (Nil, _) => l
    case (Cons(_, t), n) => drop(t, n - 1)
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = {

    @annotation.tailrec
    def go(acc: List[A], current: List[A]): List[A] = current match {
      case Nil => throw new Exception("Empty list")
      case Cons(_, Nil) => acc
      case Cons(h, t) => go(append(acc, List(h)), t)
    }

    go(Nil, l)
  }

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11 - 1/3
  def sumL(as: List[Int]) = foldLeft(as, 0)(_ + _)

  // Exercise 3.11 - 2/3
  def productL(as: List[Double]) = foldLeft(as, 1.0)(_ * _)

  // Exercise 3.11 - 3/3
  def lengthL[A](as: List[A]) = foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  def reverse[A](as: List[A]) = foldRight(as, List[A]())((h, acc) => append(acc, List(h)))

  // TODO: Exercise 3.13 - 1/2
  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???

  // TODO: Exercise 3.13 - 2/2
  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = ???

  // Exercise 3.14
  def appendR[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  // Exercise 3.15
  /*def concat[A](as: List[List[A]]): List[A] = {
    def go(l: List[List[A]], acc: List[A]): List[A] = l match {
      case Nil => acc
      //case Cons(h, t) => go(t, append(acc, h))
      case Cons(h, t) => go(t, foldRight(acc, h)((a, b) => Cons(a, b)))
    }

    go(as, List[A]())
  }*/
  def concat[A](as: List[List[A]]): List[A] = foldRight(as, Nil: List[A])(append)

  // Exercise 3.16
  def addOne(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // Exercise 3.17
  def doubleToString(as: List[Double]): List[String] = foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))
}