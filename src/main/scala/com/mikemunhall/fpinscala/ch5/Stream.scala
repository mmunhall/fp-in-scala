package com.mikemunhall.fpinscala.ch5

import com.mikemunhall.fpinscala.ch5.Stream.{cons, empty}

sealed trait Stream[+A] {
  def headOptionM: Option[A] = this match {
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

  // Exercise 5.3 - Using matcher
  def takeWhileM(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhileM(f))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhile(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (f(a)) cons(a, b) else empty)

  // Exercise 5.6
  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  // Exercise 5.7 - 1/4
  def map[B](f: A => B): Stream[B] = foldRight(Stream[B]())((a, b) => cons(f(a), b))

  // Exercise 5.7 - 2/4
  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (f(a)) cons(a, b) else b)

  // Exercise 5.7 - 3/4
  def append[B >: A](el: => Stream[B]): Stream[B] =
    foldRight(el)((a, b) => cons(a, b))

  // Exercise 5.7 - 4/4
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((a, b) => f(a) append b)

  def find(f: A => Boolean): Option[A] = filter(f) headOption

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

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def go(n: Int, m: Int): Stream[Int] = cons(n, go(n + m, n))
    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  // Exercise 5.12 - 1/4
  def fibsU: Stream[Int] = unfold((0, 1))(a => Some(a._1, (a._2, a._1 + a._2)))

  // Exercise 5.12 - 2/4
  def fromU(n: Int): Stream[Int] = unfold(0)(a => Some(a, a + 1))

  // Exercise 5.12 - 3/4
  def constantU[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  // Exercise 5.12 - 4/4
  val onesU: Stream[Int] = constantU(1)
}

