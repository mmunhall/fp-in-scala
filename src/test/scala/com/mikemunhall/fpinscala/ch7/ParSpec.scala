package com.mikemunhall.fpinscala.ch7

import java.util.concurrent.{ExecutorService, ForkJoinPool}

import org.specs2.mutable.Specification

class ParSpec extends Specification {

  "parFilter" >> {
    val par = Par.parFilter(List(1, 2, 3, 4))(_ >= 2)
    Par.run(new ForkJoinPool())(par).get === List(2, 3, 4)
  }
}
