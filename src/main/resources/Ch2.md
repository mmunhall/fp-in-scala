Chapter 2 Notes
===============

* Higher order function take other functions and/or return functions
* Object is a singleton. It declares a class and creates an object of that class in one step.
* Convention: Return Unit to hint that a method has a side effect.
* An object whose primary purpose to give its members a namespace is sometimes called a module.
* partial1, curry, uncurry and compose are all fun exercises, but how is it
applied in real world functional programming?
* There are no "operators" in Scala. Only methods.
    * Infix notation: `2 + 1`
    * Dot notation: `2.+(1)`
* Functions are values.
* `go()` or `loop()`, conventionally, are the names of helper methods used in recursive functions.
* When a recursive call is in _tail position_, Scala provides an optimization called _tail call elimination_ that prevents unnecessary stack frames from piling up. (`@annotation.tailrec`)
* Monomorphic functions operate on only one type of data.
* Polymorphic functions operate on any type of data (generally) - "Abstract over the type"
* anonymous function == function literal
* Scala converts function literals, e.g. `(a: Int, b:Int): Boolean => a < b`, to FunctionN:

    val fn = new Function2[Int, Int, Boolean] {
        def apply(a: Int, b: Int) = a < b
    }

* Sometimes there is only one implementation possible given a particular polymorphic signature. See exerceses.
* Polymorphic higher-order functions are applicable because they say nothing of the domain and simply abstract over a common pattern that occurs in many contexts.