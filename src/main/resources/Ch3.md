Chapter 3 Notes
===============

Exercise 3.1
------------

What will be result of

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    // 3 (the third case)

Exercise 3.7
------------

foldRight provides no way to immediately halt execution if any condition is met. One way to implement this might be to re-implement foldRight with a "halt" condition passed as an argument. This is probably the naive approach.

After reading the blue book solution, I was flat out dumb-wrong. We can't short-circuit and exit because foldRight, the argument to the function, is invoked _before_ our function is invoked. Also laziness will supposedly going to help with this as well.

Notes
-----

* The exercises were helpful, but how do you get in the _mindset_ to program this way? For example, I was able to find the solution for foldLeft() because the signature _told_ me how to implement it. But, how does a programmer come up with that signature to begin with?
* Helpful definition of ADT: We say that the data type is the sum or union of its data constructors, and each data constructor is the product of its arguments, hence the name algebraic data type.
* fold() can be used to implement any recursive function that would otherwise use pattern matching.

foldRight vs foldLeft
---------------------

_Hint: The name of the function tells you which argument is the accumulator._

foldRight

    f(a, f(b, f(c, z)))

foldLeft

    f(f(f(z, a), b), c)