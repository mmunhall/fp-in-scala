/*object Play {

val intList = Gen.listOf(Gen.choose(0, 100))
val prop =
  forAll(intList)(ns => ns.reverse.reverse == ns) &&
  forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

def listOf[A](a: Gen[A]): Gen[List[A]]

def forAll[A](a: Gen[A])(f: A => Boolean): Prop
z

}

object Prop {
  type SuccessCount = Int
}

trait Prop {
  def check: Either[???, SuccessCount]
  def &&(p: Prop): Prop = new Prop {
    Prop.this.check && p.check
  }
}
 */
