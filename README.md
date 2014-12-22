# Operational Transformation

This is my implementation of Operational Transformation in Scala.

NB: This is a pretty naive implementation and much of it doesn't look like idiomatic Scala. My first goal is to understand the algorithms and get them working, and do optimization later.

It operates strictly on plaintext documents, and implements operations composed using the following basic components:

* `Retain(n: Int)` - advance the cursor in the document, skipping over `n` characters
* `Insert(chars: String)` - insert `chars` at the current position of the cursor
* `Delete(n: Int)` - delete `n` characters from the cursor's current position
