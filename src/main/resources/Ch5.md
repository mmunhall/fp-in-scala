Chapter 5 Notes
===============

* non-strictness ~= laziness
* non-strict function takes its arguments by name rather than by value.
* thunk == unevaluated form of an expression
* lazy keyword delays evaluation until the result is needed, then caches the result so evaluation happens only once.
* stream ~= lazy list
* forced == invoked

Question for group
------------------

### 5.2 - drop() ###

1. What does it mean when the companion book says that unlike `take()`, `drop()` must traverse the first n elements _eagerly_.

2. Why is the function tail recursive when the function is marked `final`?