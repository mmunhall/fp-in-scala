package com.mikemunhall.fpinscala.ch10

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

// Exercise 10.5
object MonoidPropSpec extends Properties("Monoid Laws") {

  property("monoid laws") = {
    propCheck(Monoids.stringMonoid)
    propCheck(Monoids.intAddition)
    propCheck(Monoids.booleanAnd)
    propCheck(Monoids.booleanOr)
    propCheck(Monoids.optionMonoid[Int])
    propCheck(Monoids.optionMonoid[String])
    propCheck(Monoids.optionMonoid[Boolean])
  }

  def propCheck[A](monoid: Monoid[A])(implicit arbA: Arbitrary[A]) = {
    forAll { (s1: A, s2: A) =>
      monoid.op(s1, monoid.zero) == s1
      monoid.op(s2, monoid.zero) == s2
      monoid.op(monoid.op(s1, s2), monoid.zero) == monoid.op(monoid.zero, monoid.op(s1, s2))
    }
  }

}