package com.mikemunhall.fpinscala.ch10

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, cnt, r)) => Part(c + l, cnt, r)
      case (Part(l, cnt, r), Stub(d)) => Part(l, cnt, r + d)
      case (Part(cl, cc, cr), Part(dl, dc, dr)) =>
        Part(cl, cc + (if ((cr + dl).isEmpty) 0 else 1) + dc, dr)
    }
    def zero = Stub("")
  }
}