package com.mikemunhall.fpinscala.ch10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  // Exercise 10.1 = 2/4
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero = 0
  }

  // Exercise 10.1 - 2/4
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero = 1
  }

  // Exercise 10.1 - 3/4
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero = true
  }

  // Exercise 10.1 - 4/4
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero = false
  }

  // Exercise 10.2
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero = m.zero
  }

  // Exercise 10.3
  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    def zero: A => A = a => a
  }

  // Exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, b) => m.op(acc, f(b)))

  // Exercise 10.6 - 1/2
  def foldRightUsingFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // Exercise 10.6 - 2/2
  def foldLeftUsingFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // Exercise 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
    case _ if v.isEmpty => m.zero
    case item if v.length == 1 => m.op(f(item(0)), m.zero);
    case _ =>
      val (left, right) = v.splitAt(v.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
  }

  // Exercise 10.9
  type IntSegment = Option[(Int, Int, Boolean)]

  def intOrderedSegmentMonoid = new Monoid[IntSegment] {
    def op(a: IntSegment, b: IntSegment) = (a, b) match {
      case (Some((aStart, aEnd, aIsOrdered)), Some((bStart, bEnd, bIsOrdered))) =>
        Some(aStart min bStart, aEnd min bEnd, aIsOrdered && bIsOrdered && aEnd <= bStart)
      case (Some(seg), None) => Some(seg)
      case (None, Some(seg)) => Some(seg)
      case (None, None) => None
    }
    def zero = None
  }

  def isOrdered(v: IndexedSeq[Int]): Boolean =
    foldMapV(v, intOrderedSegmentMonoid)(a => Some((a, a, true))).forall(_._3)
  // End Exercise 10.9



}
