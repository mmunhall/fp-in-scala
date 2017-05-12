package com.mikemunhall.fpinscala.ch7

sealed trait Par[+A] {

  // Exercise 7.1
  def map2[B, C](par2: Par[B])(f: (A, B) => C): Par[C] = ???

}
