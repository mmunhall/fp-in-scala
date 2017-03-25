package com.mikemunhall.fpinscala.ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29 1/1
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  // Exercise 3.29 1/2
  def sizeF[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1)(1 + _ + _)
  def maximumF[A](t: Tree[Int]): Int = fold[Int, Int](t)(a => a)(_ max _)
  def depthF[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1)((l, r) => 1 + (l max r))

  // Shitty: The return type has to be specified on f, otherwise it won't compile. This is a Scala type inference thing.
  def mapF[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}