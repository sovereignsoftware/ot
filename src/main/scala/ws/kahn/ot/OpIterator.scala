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

case class OpIterator(ops: IndexedSeq[Operation]) {

  if (ops.isEmpty) { throw new OpIteratorException("Both arrays must be non-empty.") }

  /**
   * Tracks the index of the "current" element in the vector.
   */
  private var idx = 0

  /**
   * Tracks our current offset within the "current" element, for example
   * if we took only a portion of the element, the offset points to the
   * start of what's remaining.
   */
  private var offset = 0

  /**
   * Peek at the length of the next object.
   */
  def peekLength: Int = {
    ops(idx).length - offset
  }

  /**
   * Reveals the type of the next available object
   */
  def peekType: Int = {
    if (idx >= ops.length) {
      Operation.Types.Retain
    }
    else {
      ops(idx) match {
        case op: Retain => Operation.Types.Retain
        case op: Insert => Operation.Types.Insert
        case op: Delete => Operation.Types.Delete
      }
    }
  }

  /**
   * Returns whether there is another operation available to be taken.
   */
  def hasNext: Boolean = idx < ops.length && peekLength > 0

  /**
   * If a length isn't given, take the whole available operation.
   */
  def next(): Operation = next(peekLength)

  /**
   * Return a slice of the next operation equal to "length".
   *
   * ex: If the next operation is Insert("Hello"), we can request only next(2).
   *     In this case, we will return Insert("He") and the offset will be pointed
   *     at the third position.
   */
  def next(length: Int): Operation = {

    if (idx >= ops.length) {
      throw OpIteratorException("Cannot get next element when there are no more elements.")
    }

    val lengthToTake = {
      if (length > peekLength) {
        peekLength
      }
      else {
        length
      }
    }

    val nextOp = ops(idx) match {
      case op: Retain => Retain(lengthToTake, op.attributes)
      case op: InsertText => InsertText(op.chars.slice(offset, offset + length), op.attributes)
      case op: InsertCode => op
      case op: Delete => Delete(lengthToTake)
    }

    // Update offset and index as required
    offset += length
    if (offset >= ops(idx).length) {
      offset = 0
      idx += 1
    }

    nextOp
  }

}

case class OpIteratorException(msg: String) extends Exception
