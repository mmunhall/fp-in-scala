package com.mikemunhall.fpinscala

import ch5._

object Scratch extends App {

  val as = Stream(1, 2, 3)
  val bs = Stream(1, 2)
  val cs = Stream(1, 3)
  val ds = Stream(4, 5)

  println(as.zipAll(bs).takeWhile(g => g._1 equals g._2).toList.size == bs.toList.size)
  println(as.zipAll(cs).takeWhile(g => g._1 equals g._2).toList)
  println(cs.zipAll(ds).takeWhile(g => g._1 equals g._2).toList)
  println(as.zipAll(cs).takeWhile(g => g._1 equals g._2).toList)

}
