Chapter 4 Notes
===============

* Implementing the functions for 4.1 was easy when pattern matching. I struggled when converting my pattern matching functions to non-pattern matching functions. I had to cheat a bit and glance at the solutions. Even after figuring out the solutions, it still DID not feel natural or intuitive, and that's frustrating. Even when reviewing the functions I still have to concentrate to figure out what they're doing.
* There seems to be info missing in order to work on exercise 4.2?
* 4.2 was challenging. First, implement mean(). I used the standard Scala List collection so I could get the sum and length of a List[Double]. Then, implement variance() in steps (using vals) for each part of the algorithm. Finally, Piece each part together using flatMap.
* 4.5 was a bitch.
* `sequence` flips (i.e, `F[G[A]]` to `G[F[A]]`)
* `traverse` is `map` then `sequence`

Exercise 4.8
------------

A naive approach might be to nest `Either`s. That would become unwieldy quickly. A better approach might be to have the `Left` type be a list of some sort. I believe this is what Scalaz's `Validation` is.