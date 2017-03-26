package com.mikemunhall.fpinscala.ch4

// Exercise 4.2
object Variance {
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(d => math.pow(d - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _ => Some(xs.sum / xs.length)
  }
}
