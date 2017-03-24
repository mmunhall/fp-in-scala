package com.mikemunhall.fpinscala.ch3

import org.specs2.mutable.Specification

class TreeSpec extends Specification {

  "size" >> {
    "calculates the size of a tree" >> {
      Tree.size(Branch(Leaf("a"), Leaf("a"))) === 3
      Tree.size(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))) === 5
    }
  }

}
