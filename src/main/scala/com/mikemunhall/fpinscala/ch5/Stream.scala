package com.mikemunhall.fpinscala.ch5

import com.mikemunhall.fpinscala.ch5.Stream.{cons, empty}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5.1 (not stack safe)
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  // Exercise 5.1 (tail recursive/stack safe)
  def toList: List[A] = {

    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  // Exercise 5.2 - 1/1 - Imperfect (reverses elements)
  def takeR(n: Int): Stream[A] = {

    @annotation.tailrec
    def go(o: Int, cur: Stream[A], acc: Stream[A]): Stream[A] = (o, cur) match {
      case (p, _) if p == n => acc
      case (_, Empty) => acc
      case (_, Cons(h, t)) => go(o + 1, t(), Cons(h, () => acc))
    }

    go(0, this, Stream())
  }

  // Exercise 5.2 - 1/1
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // Exercise 5.2 - 1/2
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile(f))
    case _ => empty
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}