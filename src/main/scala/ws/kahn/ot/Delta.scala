package ws.kahn.ot

import play.api.libs.json._
import play.api.libs.functional.syntax._
import ws.kahn.ot.exceptions.IncompatibleDeltasException
import scala.annotation.tailrec

/**
 * What we call an delta is essentially a list of deltas on a text.
 *
 * @param operations the list of deltas in order. Deltas can be any
 *                   one of:
 *                   - retain: advance the current position, skipping over characters
 *                   - insert: insert characters at the current position
 *                   - delete: delete characters at the current position
 * @param baseLength the length of the string that this delta operates on
 */
case class Delta(operations: IndexedSeq[Operation], baseLength: Int) {

  /**
   * The length of the document after applying this delta.
   */
  val targetLength = {
    var sum = 0
    operations.map({
      case retain: Retain => retain.num
      case insert: Insert => insert.chars.length
      case delete: Delete => 0
    }).foreach(sum += _)
    sum
  }

  def :+(operation: Operation): Delta = {
    Delta(operations :+ operation, baseLength)
  }

  def +:(operation: Operation): Delta = {
    Delta(operation +: operations, baseLength)
  }

  override def toString: String = {
    s"""
       |Delta(
       |  ${operations.map(_.toString).mkString(",\n  ")}
       |)
     """.stripMargin
  }

  /**
   * Apply the delta to a document.
   *
   * @param document
   * @return
   */
  def applyTo(document: String): String = {
    var cursor = 0
    var newDocument = document

    // Iterate over the operations of this delta and apply them to the document
    for (operation <- this.operations) {
      operation match {
        case retain: Retain => {
          cursor += retain.num
        }

        case insert: Insert => {
          newDocument = newDocument.substring(0, cursor) + insert.chars + newDocument.substring(cursor)
          cursor += insert.chars.length
        }

        case delete: Delete => {
          newDocument = newDocument.substring(0, cursor) + newDocument.substring(cursor + delete.num.toInt)
        }
      }
      //println(s"Delta 1. Cursor=$cursor. Document=$newDocument")
    }

    if (cursor != newDocument.length) {
      println("Was there an error? Cursor isn't pointing to the end of the document.")
    }

    newDocument
  }

  /**
   * Composes two deltas together into one single delta.
   *
   * @param that
   * @return
   */
  def o(that: Delta): Delta = this.compose(that)

  /**
   * Composes two deltas together into one single delta.
   *
   * @param that
   * @return
   */
  def compose(that: Delta): Delta = {
    if (this.targetLength != that.baseLength) {
      throw IncompatibleDeltasException(s"The target length of the left delta (${this.targetLength}) must be equal to the base length of the right delta (${that.baseLength}). These deltas cannot be composed.")
    }

    // Handle some simple base cases
    if (this.operations.length == 0) {
      that
    }
    else if (that.operations.length == 0) {
      this
    }
    else {
      // Get an ops iterator for each delta's operations
      thisItr = OpIterator(this.operations)
      thatItr = OpIterator(that.operations)

      // Start a buffer for the newly created operations
      var operations = IndexedSeq[Operation]()

      while (thisItr.hasNext() && thatItr.hasNext()) {
        if (thatItr.peekType() == Operations.Types.Insert) {
          operations = operations :+ thatItr.next()
        }
        else if (thisItr.peekType() == Operations.Types.Delete) {
          operations = operations :+ thisItr.next()
        }
        else {
          val length = math.min(thisItr.peekLength(), thatItr.peekLength())
          val thisOp = thisItr.next(length)
          val thatOp = thatItr.next(length)

          (thisOp, thatOp) match {
            case (retainL: Retain, retainR: Retain) => operations = operations :+ Retain(length, retainL.attributes.deepMerge(retainR.attributes)) // need to compose attributes here
            case (retainL: Retain, deleteR: Delete) => operations = operations :+ Delete(length, retainL.attributes.deepMerge(deleteR.attributes))
            case (insertL: Insert, retainR: Retain) => operations = operations :+ Insert(insertL.chars, insertL.attributes.deepMerge(retainR.attributes)) // compose attributes
            case (insertL: Insert, deleteR: Delete) => // Do nothing, they cancel each other out.
          }
        }
      }

      // Build the composed delta
      val composedOp = Delta(operations, this.baseLength)

      // Verify that the target length matches that of the "right" delta.
      if (composedOp.targetLength != that.targetLength) {
        throw CompositionErrorException(s"Composition error. The target length of the composed delta (${composedOp.targetLength}) did not equal the target length of the right delta (${that.targetLength})!")
      }

      composedOp
    }
  }

  /**
   * Transform another delta against this one.
   *
   * This delta has priority over that delta, unless the priority flag is set
   * to true, in which case that delta wins.
   *
   * @param that the other operation to transform this one against
   * @param priority whether this operation takes priority. By default this operation is being
   *                 applied _second_.
   * @return
   */
  def transform(that: Delta, priority: Boolean = false): Delta = {
    val thisItr = OpIterator(this.operations)
    val thatItr = OpIterator(that.operations)

    var xfOps = IndexedSeq.empty[Operation]

    while (leftItr.hasNext() || rightItr.hasNext()) {
      if (thisItr.peekType() == Operation.Types.Insert &&
          (priority || thatItr.peekType() != Operation.Types.Insert)
      ) {
        xfOps = xfOps :+ Retain(thisItr.next().length)
      }
      else if (thatItr.peekType() == Operation.Types.Insert) {
        xfOps = xfOps :+ thatItr.next()
      }
      else {
        val length = math.min(thisItr.peekLength(), thatItr.peekLength())
        val thisOp = thisItr.next(length)
        val thatOp = thatItr.next(length)

        (thisOp, thatOp) match {
          case (delete: Delete, _) => // do nothing
          case (_, delete: Delete) => xfOps = xfOps :+ delete
          case _ => xfOps = xfOps :+ Retain(length, thisOp.attributes.deepMerge(rightOp.attributes))
        }
      }
    }

    Delta(xfOps, this.targetLength)
  }

  /**
   * Transform a position (ie: cursor position within a document) against this
   * delta. For example, if a user submits their current cursor position, and other
   * deltas have been applied more recently, their cursor position can be transformed.
   */
  def transformPosition(position: Int, priority: Boolean = false) = {
    val thisItr = OpIterator(this.operations)
    var index = position
    var offset = 0

    while (thisItr.hasNext() && offset <= position) {
      val length = thisItr.peekLength()
      val nextType = thisItr.peekType()

      thisItr.next()

      if (nextType == Operations.Types.Delete) {
        index -= math.min(length, index - offset)
      }
      else if (nextType == Operations.Types.Insert && (offset < index || !priority)) {
        index += length
        offset += length
      }
      else {
        offset += length
      }
    }
    index
  }

  /**
   * An alias for the transform method.
   *
   * @param that
   * @return
   */
  def x(that: Delta): Delta = this.transform(that)
}

object Delta {

  implicit val reads: Reads[Delta] = (
    (__ \ "operations").read[IndexedSeq[Operation]] and
      (__ \ "baseLength").read[Int]
    )(Delta.apply _)

  implicit val writes: Writes[Delta] = (
    (__ \ "operations").write[IndexedSeq[Operation]] and
      (__ \ "baseLength").write[Int]
    )(unlift(Delta.unapply))

}
