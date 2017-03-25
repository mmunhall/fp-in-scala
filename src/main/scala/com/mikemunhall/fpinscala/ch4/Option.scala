package com.mikemunhall.fpinscala.ch4

sealed trait Option[+A] {
  // Exercise 4.1 - 1/5
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  // Exercise 4.1 - 2/5
  // TODO: Rewrite without matching
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  // Exercise 4.1 - 3/5
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  // Exercise 4.1 - 4/5
  def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  // Exercise 4.1 - 5/5
  def filter(f: A => Boolean): Option[A] = ???
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]
