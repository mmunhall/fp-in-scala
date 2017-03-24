package com.mikemunhall.fpinscala.ch3

import org.specs2.mutable.Specification

class TreeSpec extends Specification {

  "size" >> {
    "calculates the size of a tree" >> {
      Tree.size(Branch(Leaf("a"), Leaf("a"))) === 3
      Tree.size(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))) === 5
    }
  }

  "maximum" >> {
    "finds the highest value in an Int tree" >> {
      Tree.maximum(Branch(Leaf(2), Leaf(1))) === 2
      Tree.maximum(Branch(Leaf(1), Leaf(2))) === 2
      Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) === 3
      Tree.maximum(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) === 3
      Tree.maximum(Branch(Leaf(3), Branch(Leaf(1), Leaf(1)))) === 3
    }
  }

  "depth" >> {
    "calculates the depth of the deepest node" >> {
      Tree.depth(Branch(Leaf("a"), Leaf("a"))) === 2
      Tree.depth(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))) === 3
      Tree.depth(Branch(Leaf("a"), Branch(Leaf("b"), Branch(Leaf("c"), Leaf("d"))))) === 4
      Tree.depth(Branch(Branch(Leaf("b"), Branch(Leaf("c"), Leaf("d"))), Leaf("a"))) === 4
    }
  }
}
