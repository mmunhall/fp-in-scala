package com.mikemunhall.fpinscala.ch3

import org.specs2.mutable.Specification

class TreeSpec extends Specification {

  "size" >> {
    "calculates the size of a tree" >> {
      Tree.size(Branch(Leaf("a"), Leaf("a"))) === 3
      Tree.size(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))) === 5
      Tree.size(Branch(Branch(Leaf("b"), Branch(Leaf("c"), Leaf("d"))), Leaf("a"))) === 7
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

  "map" >> {
    "converts a tree from type to type B" >> {
      Tree.map(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)))(_.toDouble) ===
        Branch(Branch(Leaf(1.0), Branch(Leaf(2.0), Leaf(3.0))), Leaf(4.0))
    }
  }

  "sizeF" >> {
    "calculates the size of a tree using fold" >> {
      Tree.sizeF(Branch(Leaf("a"), Leaf("a"))) === 3
      Tree.sizeF(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))) === 5
      Tree.sizeF(Branch(Branch(Leaf("b"), Branch(Leaf("c"), Leaf("d"))), Leaf("a"))) === 7
    }
  }

  "maximumF" >> {
    "finds the highest value in an Int tree using fold" >> {
      Tree.maximumF(Branch(Leaf(2), Leaf(1))) === 2
      Tree.maximumF(Branch(Leaf(1), Leaf(2))) === 2
      Tree.maximumF(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) === 3
      Tree.maximumF(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) === 3
      Tree.maximumF(Branch(Leaf(3), Branch(Leaf(1), Leaf(1)))) === 3
    }
  }

  "depthF" >> {
    "calculates the depth of the deepest node using fold" >> {
      Tree.depthF(Branch(Leaf("a"), Leaf("a"))) === 2
      Tree.depthF(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))) === 3
      Tree.depthF(Branch(Leaf("a"), Branch(Leaf("b"), Branch(Leaf("c"), Leaf("d"))))) === 4
      Tree.depthF(Branch(Branch(Leaf("b"), Branch(Leaf("c"), Leaf("d"))), Leaf("a"))) === 4
    }
  }

  "mapF" >> {
    "converts a tree from type to type B using fold" >> {
      Tree.mapF(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)))(_.toDouble) ===
        Branch(Branch(Leaf(1.0), Branch(Leaf(2.0), Leaf(3.0))), Leaf(4.0))
    }
  }
}
