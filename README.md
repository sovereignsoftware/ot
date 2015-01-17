# Operational Transformation

This is my implementation of Operational Transformation in Scala. It is a rich-text algorithm that aims to be compatible with the rich-text OT type written in JavaScript here: https://github.com/ottypes/rich-text. Some of my code was adapted from this library.

It operates on plain-text documents with added "attributes" to provide rich-text annotations.

* `Retain(n: Int, attributes: Option[Map[String, Attribute]])` - advance the cursor in the document, skipping over `n` characters
* `Insert(chars: String, attributes: Option[Map[String, Attribute]])` - insert `chars` at the current position of the cursor
* `Delete(n: Int)` - delete `n` characters from the cursor's current position

The attribute list is an optional mapping from string keys to `Attribute` values. An `Attribute` can
be any one of:

* `StringAttribute(value: String)` -- corresponds to javascript strings and the Play JSON "JsString" object
* `NumberAttribute(value: Double)` -- corresponds to javascript numbers and the Play JSON "JsNumber" object
* `BooleanAttribute(value: Boolean)` -- corresponds to javascript booleans and the Play JSON "JsBoolean" object
* `NullAttribute()` -- this is the special one. I want to treat "Null" as an actual value, since this is a special case for Retain ops that will remove an attribute key. This
corresponds to javascript's "null" and the Play JSON "JsNull" object

I have implemented the two crucial functions on operations: compose, and transform.

* `val composedDelta: Operation = delta1.compose(delta2)` - used between two operations that should be applied in sequence from the same starting document, this method composes them together into a single operation. This is useful for simplifying operations before they are to be transformed... rather than transforming 5 x 5 operations you can compose each side and transform 1 x 1 operation.
* `val xfDelta2 = delta1.transform(delta2)` - given two operations that should be applied in parallel on the same starting document, this will transform both operations returning them in a tuple: the server's operation will be transformed to be applied to the client's document, and the client's operation will be transformed to be applied on the server. Since all operations must still be ordered, we assume that the server's changes are applied "first" and in any conflict the server "wins".

## Usage

The library can be imported into your project with SBT. First, add my repository to your resolvers:

    resolvers ++= Seq(
      // ...other resolvers...
      "kahn.ws" at "https://repo.kahn.ws/maven/snapshots"
    )
    
And then add the dependency:

    libraryDependencies ++= Seq(
      // ...your other dependencies...
      "ws.kahn" %% "ot" % "1.0-SNAPSHOT"
    )
    
Currently only the 1.0-SNAPSHOT release has been published, and it's compiled for both Scala 2.10 and 2.11. As this is a little personal experiment / work in progress, may not work as intended, or at all.
