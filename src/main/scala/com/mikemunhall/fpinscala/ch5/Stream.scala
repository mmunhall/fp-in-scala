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

  // Exercise 5.13 - 1/5
  def mapU[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  // Exercise 5.13 - 2/5
  def takeU(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (n, Cons(h, t)) if n > 0 => Some(h(), (n - 1, t()))
    case _ => None
  }

  // Exercise 5.13 - 3/5
  def takeWhileU(f: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }

  // Exercise 5.13 - 4/5
  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold(this, bs) {
      case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
      case _ => None
    }

  // Exercise 5.13 - 5/5
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold(this, s2) {
      case (Cons(ah, at), Cons(bh, bt)) => Some(((Some(ah()), Some(bh())), (at(), bt())))
      case (Cons(ah, at), empty)  => Some(((Some(ah()), None), (at(), empty)))
      case (empty, Cons(bh, bt))  => Some(((None, Some(bh())), (empty, bt())))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  // Exercise 5.14 - Mine is not quite as nice as the solution in the companion book
  def startsWithMine[A](s: Stream[A]): Boolean =
    this.zipAll(s).takeWhile(g => g._1 equals g._2).toList.size == s.toList.size

  // Exercise 5.14 - Companion book solution
  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (l, r) => l == r
    }

  // Exercise 5.15 - 1/2 - mine
  def tailsMine: Stream[Stream[A]] = Stream.unfold(this) {
    case Cons(h, t) => Some((cons(h(), t()), t()))
    case _ => None
  } append Stream(empty)

  // Exercise 5.15 - 1/2 - companion book
  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Empty => None
    case c => Some((c, c drop 1))
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

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

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

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

  // Exercise 5.12 - 1/4
  def fibsU: Stream[Int] = unfold((0, 1))(a => Some(a._1, (a._2, a._1 + a._2)))

  // Exercise 5.12 - 2/4
  def fromU(n: Int): Stream[Int] = unfold(0)(a => Some(a, a + 1))

  // Exercise 5.12 - 3/4
  def constantU[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  // Exercise 5.12 - 4/4
  val onesU: Stream[Int] = constantU(1)
}

