Chapter 8 Notes
===============

Exercise 8.1
------------

_...come up with properties that specify the implementation of a sum: `List[Int] => Int` function..._

1. Reversing a list and summing it should give the same result as summing the original, nonreversed list.
2. Changing the order of the list in any way should give the same result as summing the original list.
3. If all the elements are the same value then the sum should be the product of the first element and the length of the list.
4. The sum of an empty list should be 0.

Exercise 8.2
------------

_What properties specify a function that finds the maximum of a `List[Int]`?_

1. Changing the order of the list in any way should give the same result as finding the maximum in the original list.
2. The maximum of an empty list is **???**.
3. If all the elements are the same value then the maximum is any element in the list.
4. If more than one element are both the maximum then the maximum can be any of those elements.
5. The maximum of a list with one element is that element.
6. The maximum of a list is equal to or greater than all the elements in the list.