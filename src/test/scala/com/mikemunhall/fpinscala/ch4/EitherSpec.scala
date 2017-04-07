package com.mikemunhall.fpinscala.ch4

import org.specs2.mutable.Specification

class EitherSpec extends Specification {

  "map" >> {
    Right(1).map(_ + 1) === Right(2)
    Left("fail").map((a:Int) => a + 1) === Left("fail")
  }

  "flatMap" >> {
    Right(1).flatMap(r => Right(r)) === Right(1)
    Right(1).flatMap(r => Left("fail")) === Left("fail")
    Left("fail").flatMap(r => Right(r)) === Left("fail")
  }

  "orElse" >> {
    Right(1).orElse(Right(2)) === Right(1)
    Left("fail").orElse(Right(2)) === Right(2)
  }

  "map2" >> {
    "using map" >> {
      Right(1).map2(Right(2))(_ + _) === Right(3)
      Left("fail").map2(Right(2))((a: Int, b: Int) => a + b) === Left("fail")
      Right(1).map2(Left("fail"))(_ + _) === Left("fail")
    }

    "using for comp FTW" >> {
      Right(1).map2(Right(2))(_ + _) === Right(3)
      Left("fail").map2(Right(2))((a: Int, b: Int) => a + b) === Left("fail")
      Right(1).map2(Left("fail"))(_ + _) === Left("fail")
    }
  }

}
