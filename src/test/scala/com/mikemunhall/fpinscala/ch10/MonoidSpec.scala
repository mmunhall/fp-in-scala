package com.mikemunhall.fpinscala.ch10

import org.specs2.mutable.Specification

class MonoidSpec extends Specification {

  "foldMapV" >> {
    Monoids.foldMapV(IndexedSeq("a"), Monoids.stringMonoid)(a => a) === "a"
    Monoids.foldMapV(IndexedSeq("a", "b"), Monoids.stringMonoid)(a => a) === "ab"
    Monoids.foldMapV(IndexedSeq("a", "b", "c"), Monoids.stringMonoid)(a => a) === "abc"
    Monoids.foldMapV(IndexedSeq("a", "b", "c", "d"), Monoids.stringMonoid)(a => a) === "abcd"
  }

}
