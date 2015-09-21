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

package software.sovereign.ot

import play.api.libs.json._
import play.api.libs.functional.syntax._

sealed trait Operation {
  /**
   * @return the length of the operation: the number of characters
   * retained, inserted, or deleted.
   */
  def length: Int
}

sealed trait AttributedOperation extends Operation {
  val attributes: Option[Map[String, Attribute]]
}

object Operation {

  object Types {
    val Retain = 0
    val Insert = 1
    val Delete = 2
  }

  implicit val opReads = new Reads[Operation] {
    def reads(json: JsValue) = {
      val op = {
        (json \ "retain").asOpt[Int].map { num => Retain.retainReads.reads(json) } orElse
          (json \ "insert").asOpt[String].map { num => InsertText.insertTextReads.reads(json) } orElse
            (json \ "insert").asOpt[Int].map { num => InsertCode.insertCodeReads.reads(json) } orElse
              (json \ "delete").asOpt[Int].map { num => Delete.deleteReads.reads(json) }
      }
      op.getOrElse(JsError("Error reading operation"))
    }
  }

  implicit val opWrites = new Writes[Operation] {
    def writes(opc: Operation): JsValue = {
      opc match {
        case retain: Retain => Retain.retainWrites.writes(retain)
        case insert: Insert => Insert.insertWrites.writes(insert)
        case delete: Delete => Delete.deleteWrites.writes(delete)
      }
    }
  }
}

/**
 * The "retain" operation moves the cursor along the document by
 * a number of characters, effectively "skipping" over them.
 *
 * @param num the number of characters to skip or retain.
 */
case class Retain(num: Int, attributes: Option[Map[String, Attribute]] = None) extends AttributedOperation {
  override def toString: String = s"Retain($num, ${attributes.toString()})"
  override def length: Int = num
}
object Retain {
  implicit val retainReads = (
    (__ \ "retain").read[Int] and
      (__ \ "attributes").readNullable[Map[String, Attribute]]
    )(Retain.apply _)

  implicit val retainWrites = (
      (__ \ "retain").write[Int] and
        (__ \ "attributes").writeNullable[Map[String, Attribute]]
    )(unlift(Retain.unapply))
}

/**
 * The "insert" operation inserts a given string at the cursor's current
 * position in the string.
 */

sealed trait Insert extends AttributedOperation {
  def length: Int
}
object Insert {
  implicit val insertReads = new Reads[Insert] {
    def reads(json: JsValue) = {
      json \ "insert" match {
        case JsString(text) => InsertText.insertTextReads.reads(json)
        case JsNumber(code) => InsertCode.insertCodeReads.reads(json)
        case _ => JsError("Badly formatted Insert operation.")
      }
    }
  }
  implicit val insertWrites = new Writes[Insert] {
    def writes(insert: Insert) = {
      insert match {
        case text: InsertText => InsertText.insertTextWrites.writes(text)
        case code: InsertCode => InsertCode.insertCodeWrites.writes(code)
      }
    }
  }
}

case class InsertText(chars: String, attributes: Option[Map[String, Attribute]] = None) extends Insert {
  override def toString: String = s"""InsertText(\"${chars}\", ${attributes.toString()})"""
  override def length: Int = chars.length
}
object InsertText {
  implicit val insertTextReads = (
    (__ \ "insert").read[String] and
      (__ \ "attributes").readNullable[Map[String, Attribute]]
    )(InsertText.apply _)

  implicit val insertTextWrites = (
    (__ \ "insert").write[String] and
      (__ \ "attributes").writeNullable[Map[String, Attribute]]
    )(unlift(InsertText.unapply))
}

case class InsertCode(code: Int, attributes: Option[Map[String, Attribute]] = None) extends Insert {
  override def toString: String = s"""InsertCode(\"${code}\", ${attributes.toString()})"""
  override def length: Int = 1
}
object InsertCode {
  implicit val insertCodeReads = (
    (__ \ "insert").read[Int] and
      (__ \ "attributes").readNullable[Map[String, Attribute]]
    )(InsertCode.apply _)

  implicit val insertCodeWrites = (
    (__ \ "insert").write[Int] and
      (__ \ "attributes").writeNullable[Map[String, Attribute]]
    )(unlift(InsertCode.unapply))
}



/**
 * The "delete" operation deletes a number of characters starting
 * from the cursor's current position in the document.
 *
 * @param num the number of characters to delete.
 */
case class Delete(num: Int) extends Operation {
  override def toString: String = s"Delete($num)"
  override def length: Int = num
}

object Delete {
  implicit val deleteReads = new Reads[Delete] {
    def reads(json: JsValue) = {
      (json \ "delete").asOpt[Int] match {
        case Some(delete) => JsSuccess(Delete(delete))
        case _ => JsError("Invalid delete operation.")
      }
    }
  }

  implicit val deleteWrites = new Writes[Delete] {
    def writes(opc: Delete): JsValue = {
      Json.obj("delete" -> JsNumber(opc.num))
    }
  }
}
