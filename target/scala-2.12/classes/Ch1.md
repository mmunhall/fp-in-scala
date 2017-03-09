Chapter 1 Notes
===============

* Programs use only pure functions (no side effects)
* Pure functions are easier to
  * test
  * reuse,
  * parallelize,
  * generalize
  * reason about
  * less prone to bugs
* In the real world, we have side effects (IO, network, database, etc.). We push these operations to the "outside" in FP programs.
* When a function has no observable effect on the execution of the program other than to compute a result given its inputs, we say that it has no side effects.
* Functions that have side effects are sometimes called _procedures_.
* Pure functions (and pure expressions, too) are formalized by the notion of _referential transparency_.
  * An expression can be replaced by its result without changing the meaning of the program.
  * A function is pure if calling with referentially transparent arguments is also referentially transparent.
  * Everything a function _does_ is represented by the value it returns.
* The _substitution model_ is a method where we substitute parts of an equation with their value, much like an algebraic equation.
