package com.mikemunhall.fpinscala.ch9

import scala.language.higherKinds

trait Parsers[ParseError, Parser[+_]] { self =>

  def char(c: Char): Parser[Char]                                // run(char(c))(c.toString) == Right(c)
  implicit def string(s: String): Parser[String]                 // run(string(s))(s) == Right(s)
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A];            // run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
                                                                 // run(or(string("abra"), string("cadabra")))("cadabra") == Right("abra")
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]          // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
                                                                 // run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
                                                                 // run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

}
