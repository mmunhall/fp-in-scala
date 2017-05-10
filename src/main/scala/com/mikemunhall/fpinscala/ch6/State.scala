package com.mikemunhall.fpinscala.ch6

// Exercise 6.10
case class State[S, +A](run: S => (S, A)) {

  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (s1, a) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (s, a))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]())) { (a, acc) =>
      a.map2(acc)(_ :: _)
    }
}