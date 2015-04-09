// Copyright 2015 Chris Kahn
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
//   limitations under the License.

package ws.kahn.ot

import play.api.libs.json._
import play.api.libs.functional.syntax._
import ws.kahn.ot.exceptions.IncompatibleDeltasException
import scala.annotation.tailrec

/**
 * A delta represents a change performed on a document as a sequence of operations
 * that move a cursor over the document.
 *
 * @param operations the list of operations in order. An operation can be any
 *                   one of:
 *                   - retain: advance the current position, skipping over characters
 *                   - insert: insert characters at the current position
 *                   - delete: delete characters at the current position
 */
case class Delta(operations: IndexedSeq[Operation]) {

  /**
   * The length of the starting document this delta operates on. Equal
   * to the sum of the lengths of its retain and delete operations.
   */
  val baseLength = {
    operations.map({
      case retain: Retain => retain.length
      case insert: Insert => 0
      case delete: Delete => delete.length
    }).sum
  }
  
  /**
   * The length of the document after applying this delta. Equal to
   * the length of its retain and insert operations.
   */
  val targetLength = {
    operations.map({
      case retain: Retain => retain.num
      case insert: Insert => insert.length
      case delete: Delete => 0
    }).sum
  }

  /**
   * Is the delta a "document"? If a delta contains only inserts, then it can be said
   * that it represents the state of a document itself.
   */
  val isDocument: Boolean = operations.filter({
    case op: Retain => true
    case op: Delete => true
    case op: Insert => false
  }).isEmpty

  /**
   * Append an operation ot this delta.
   *
   * @param operation
   * @return the delta with the operation appended
   */
  def :+(operation: Operation): Delta = {
    Delta(operations :+ operation)
  }

  /**
   * Prepend an operation to this delta.
   * @param operation
   * @return the delta with the operation prepended
   */
  def +:(operation: Operation): Delta = {
    Delta(operation +: operations)
  }

  /**
   * @return a string representation of the delta. For debugging purposes only.
   */
  override def toString: String = {
    s"""
       |Delta(
       |  ${operations.map(_.toString).mkString(",\n  ")}
       |)
     """.stripMargin
  }

  /**
   * Composes two deltas together, that should be applied
   * to a document in sequence, into one delta.
   *
   * @param that the second delta to be composed after this one
   * @return the composed delta
   */
  def o(that: Delta): Delta = this.compose(that)

  /**
   * Transform two deltas applied to a document simultaneously.
   *
   * @param that
   * @return
   */
  def x(that: Delta, priority: Boolean = false): Delta = this.transform(that, priority)

  /**
   * Composes two deltas together, that should be applied
   * to a document in sequence, into one delta.
   *
   * @param that the second delta to be composed after this one
   * @return the composed delta
   */
  def compose(that: Delta): Delta = {
    if (this.targetLength != that.baseLength) {
      throw IncompatibleDeltasException(this, that)
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
      val thisItr = OpIterator(this.operations)
      val thatItr = OpIterator(that.operations)

      // Start a buffer for the newly created operations
      var operations = IndexedSeq[Operation]()

      while (thisItr.hasNext || thatItr.hasNext) {
        if (thatItr.hasNext && (thatItr.peekType == Operation.Types.Insert)) {
          operations = operations :+ thatItr.next
        }
        else if (thisItr.hasNext && (thisItr.peekType == Operation.Types.Delete)) {
          operations = operations :+ thisItr.next
        }
        else if (thisItr.hasNext && thatItr.hasNext) {
          val length = math.min(thisItr.peekLength, thatItr.peekLength)
          val thisOp = thisItr.next(length)
          val thatOp = thatItr.next(length)

          (thisOp, thatOp) match {
            case (retainL: Retain, retainR: Retain) => operations = operations :+ Retain(length, Attribute.compose(retainL.attributes, retainR.attributes, true))
            case (retainL: Retain, deleteR: Delete) => operations = operations :+ Delete(length)
            case (insertL: InsertText, retainR: Retain) => operations = operations :+ InsertText(insertL.chars, Attribute.compose(insertL.attributes, retainR.attributes))
            case (insertL: InsertCode, retainR: Retain) => operations = operations :+ InsertCode(insertL.code, Attribute.compose(insertL.attributes, retainR.attributes))
            case (insertL: Insert, deleteR: Delete) => // Do nothing, they cancel each other out.
            case (_, insertR: Insert) => throw new Exception("Something went horribly wrong. Inserts on the right side should already be taken care of.")
            case (deleteL: Delete, _) => throw new Exception("Something went horribly wrong. Deletes on the left side should already be taken care of.")
          }
        }
        else {
          throw CompositionErrorException("Invalid composition: delta mismatch.")
        }
      }

      // Build the composed delta
      val composedOp = Delta(operations)

      // Verify that the target length matches that of the "right" delta.
      if (composedOp.targetLength != that.targetLength) {
        throw CompositionErrorException(s"Invalid composition: The target length of the composed delta (${composedOp.targetLength}) did not equal the target length of the right delta (${that.targetLength})!")
      }

      // Merge adjacent operations with the same type and attributes
      composedOp.optimized
    }
  }

  /**
   * Transform another delta against this one.
   *
   * This delta has priority over that delta, unless the priority flag is set
   * to true, in which case that delta wins.
   *
   * @param that the other operation to transform this one against
   * @param priority whether this delta wins contests. By default this operation is being
   *                 applied _second_.
   * @return the "that" delta, transformed
   */
  def transform(that: Delta, priority: Boolean = false): Delta = {
    val thisItr = OpIterator(this.operations)
    val thatItr = OpIterator(that.operations)

    var xfOps = IndexedSeq.empty[Operation]

    while (thisItr.hasNext || thatItr.hasNext) {
      if (thisItr.peekType == Operation.Types.Insert &&
          (priority || thatItr.peekType != Operation.Types.Insert)
      ) {
        xfOps = xfOps :+ Retain(thisItr.next.length)
      }
      else if (thatItr.peekType == Operation.Types.Insert) {
        xfOps = xfOps :+ thatItr.next
      }
      else {
        val length = math.min(thisItr.peekLength, thatItr.peekLength)
        val thisOp = thisItr.next(length)
        val thatOp = thatItr.next(length)

        (thisOp, thatOp) match {
          case (delete: Delete, _) => // do nothing

          case (_, delete: Delete) => xfOps = xfOps :+ delete

          case (thisAOp: AttributedOperation, thatAOp: AttributedOperation) =>
            xfOps = xfOps :+ Retain(length, Attribute.transform(thisAOp.attributes, thatAOp.attributes, priority))
        }
      }
    }

    // Merge adjacent operations with the same type and attributes
    Delta(xfOps).optimized
  }

  /**
   * Transform a position (ie: cursor index within a document) against this
   * delta. For example, if a user submits their current cursor position, and other
   * deltas have been applied more recently, their cursor position can be transformed.
   */
  def transformPosition(position: Int, priority: Boolean = false): Int = {
    val thisItr = OpIterator(this.operations)
    var index = position
    var offset = 0

    while (thisItr.hasNext && offset <= position) {
      val length = thisItr.peekLength
      val nextType = thisItr.peekType

      thisItr.next

      if (nextType == Operation.Types.Delete) {
        index -= math.min(length, index - offset)
      }
      else if (nextType == Operation.Types.Insert && (offset < index || !priority)) {
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
   * Optimize an operation by combining adjacent components of the same type.
   *
   * This is a pretty simple/naive function... for now it doesn't do anything
   * clever like re-ordering the components to make them more semantically correct.
   *
   * @return the optimized delta
   */
  private def optimized: Delta = {
    if (this.operations.length > 1) {
      var i = 0
      var j = 1
      var merged = IndexedSeq.empty[Operation]

      var headA = this.operations(i)
      var headB = this.operations(j)
      var shouldContinue = true

      while (shouldContinue) {
        (headA, headB) match {
          case (retainA: Retain, retainB: Retain)
            if retainA.attributes.isEmpty && retainB.attributes.isEmpty ||
              (retainA.attributes.nonEmpty && retainB.attributes.nonEmpty &&
               retainA.attributes.equals(retainB.attributes)) => {
            val newRetain = Retain(retainA.num + retainB.num)

            headA = newRetain

            j += 1

            if (j >= this.operations.length) {
              shouldContinue = false
              merged = merged :+ headA
            }
            else {
              headB = this.operations(j)
            }
          }
          case (insertA: InsertText, insertB: InsertText)
            if insertA.attributes.isEmpty && insertB.attributes.isEmpty ||
               (insertA.attributes.nonEmpty && insertB.attributes.nonEmpty &&
                insertA.attributes.equals(insertB.attributes)) => {
            val newInsert = InsertText(insertA.chars + insertB.chars)
            headA = newInsert
            j += 1
            if (j >= this.operations.length) {
              shouldContinue = false
              merged = merged :+ headA
            }
            else {
              headB = this.operations(j)
            }
          }
          case (deleteA: Delete, deleteB: Delete) => {
            val newDelete = Delete(deleteA.num + deleteB.num)
            headA = newDelete
            j += 1
            if (j >= this.operations.length) {
              shouldContinue = false
              merged = merged :+ headA
            }
            else {
              headB = this.operations(j)
            }
          }
          case (anyA, anyB) => {
            merged = merged :+ anyA
            i = j
            j += 1

            if (i >= this.operations.length ||
              j >= this.operations.length) {
              shouldContinue = false
              merged = merged :+ anyB
            }
            else {
              headA = this.operations(i)
              headB = this.operations(j)
            }
          }
        }
      }

      val optimized = Delta(merged)
      optimized
    }
    else {
      this
    }
  }
}

object Delta {

  implicit val reads = new Reads[Delta] {
    def reads(json: JsValue) = {
      val opOps = (json \ "ops").as[IndexedSeq[Operation]]
      Option(opOps) match {
        case Some(operations) => JsSuccess(Delta(operations))
        case None => JsError("Invalid or missing operations list.")
      }
    }
  }

  implicit val writes = new Writes[Delta] {
    def writes(delta: Delta): JsValue = {
      Json.obj(
        "ops" -> delta.operations
      )
    }
  }

  def compose(first: Delta, second: Delta): Delta = first o second
  def transform(left: Delta, right: Delta): Delta = left x right
  def transform(left: Delta, right: Delta, priority: Boolean) = left.transform(right, priority)
}
