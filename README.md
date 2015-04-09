# Operational Transformation

This is my implementation of Operational Transformation in Scala. It is a rich-text algorithm that aims to be compatible 
with the rich-text OT type written in JavaScript here: https://github.com/ottypes/rich-text. Some of my code was adapted 
from this library. The code here is licensed under the Apache license.

This implementation extends plain-text documents with "attributes" to provide rich-text annotations. It also defines two
types of inserts, one for text and one for "codes" that allow for inserting objects into the document (such as line
breaks, images, etc).

* `Retain(n: Int, attributes: Option[Map[String, Attribute]])` - advance the cursor in the document, skipping over `n` characters
* `InsertText(chars: String, attributes: Option[Map[String, Attribute]])` - insert `chars` at the current position of the cursor
* `InsertCode(code: Int, attributes: Option[Map[String, Attribute]])` - insert the given code at the current position of the cursor
* `Delete(n: Int)` - delete `n` characters from the cursor's current position

The attribute list is an (optional) mapping from string keys to `Attribute` values. An `Attribute` can
be any one of:

* `StringAttribute(value: String)` -- corresponds to javascript strings and the Play JSON "JsString" object
* `NumberAttribute(value: Double)` -- corresponds to javascript numbers and the Play JSON "JsNumber" object
* `BooleanAttribute(value: Boolean)` -- corresponds to javascript booleans and the Play JSON "JsBoolean" object
* `NullAttribute()` -- this is the special one. Null is a value here since an attribute defined as "null" will remove
 the named annotation from characters it passes over. This corresponds to javascript's "null" and the Play JSON "JsNull" 
 object and you should only really see this on `Retain` operations (it's useless elsewhere).

I have implemented the two crucial functions on operations: compose, and transform.

* `val firstThenSecond = firstDelta.compose(secondDelta)` - used between two operations that should be applied in sequence from the same starting document, this method composes them together into a single operation. This is useful for simplifying operations before they are to be transformed... rather than transforming 5 x 5 operations you can compose each side and transform 1 x 1 operation.
* `val transformedClientDelta = serverDelta.transform(clientDelta)` - given two operations that should be applied in parallel on the same starting document, and assuming that they have been ordered by the server into a server delta (won the race) and a client delta (lost the race), this will return the client delta transformed to apply to the server's document after the server's delta has been applied 
* `val transformedServerDelta = clientDelta.transform(serverDelta, true) - given two operations that should be applied in parallel on the same starting document, and assuming that they have been ordered by the server into a server delta (won the race) and a client delta (lost the race), this will return the server delta transformed to apply to the client's document "before" the client's edit

A "document" can be represented as a delta that contains only inserts.

See the tests in `src/test/scala/OtSpec` to see the these methods in action.

## Usage

_The repository is currently down!_

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
