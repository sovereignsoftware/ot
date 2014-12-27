package ws.kahn.ot

import play.api.libs.json._
import play.api.libs.functional.syntax._

sealed trait OperationComponent {
  val opType: Char
}

object OperationComponent {
  implicit val opReads = new Reads[OperationComponent] {
    def reads(json: JsValue) = {
      (json \ "type").asOpt[String] match {
        case Some(opType) if opType == "retain" => {
          (json \ "n").asOpt[Int] match {
            case Some(num) => JsSuccess(Retain(num))
            case _ => JsError("Retain operation requires 'n' field with an integer value.")
          }
        }
        case Some(opType) if opType == "insert" => {
          (json \ "chars").asOpt[String] match {
            case Some(chars) => JsSuccess(Insert(chars))
            case _ => JsError("Insert operation requires 'chars' field with a string value.")
          }
        }
        case Some(opType) if opType == "delete" => {
          (json \ "n").asOpt[Int] match {
            case Some(num) => JsSuccess(Delete(num))
            case _ => JsError("Delete operation requires 'n' field with an integer value.")
          }
        }
        case None => JsError("Operation component field 'type' must be provided and equal to retain, insert, or delete.")
      }
    }
  }

  implicit val opWrites = new Writes[OperationComponent] {
    def writes(opc: OperationComponent): JsValue = {
      opc match {
        case retain: Retain => Json.obj(
          "type" -> "retain",
          "n" -> retain.num
        )
        case insert: Insert => Json.obj(
          "type" -> "insert",
          "chars" -> insert.chars
        )
        case delete: Delete => Json.obj(
          "type" -> "delete",
          "n" -> delete.num
        )
      }
    }
  }
}

/**
 * The "retain" operation moves the cursor along the document by
 * a number of characters, effectively "skipping" over them.
 *
 * NB: comparison methods do not work exactly as one might expect.
 *     The equals (==) operator only compares the number of affected
 *     characters! Retain(5) == Insert("kitty") is true!
 *
 *     To test whether two operations are truly "equal" (and have the same type)
 *     use the triple-equals operator (===). Retain(5) === Insert("kitty") is false
 *     but Insert("kitty") === Insert("kitty") is true!
 *
 *     The remaining comparison operators (>, >=, <, <=) also compare against
 *     the num or chars.length values.
 *
 * @param num the number of characters to skip or retain.
 */
case class Retain(num: Int) extends OperationComponent {
  override val opType: Char = 'r'
  override def toString: String = s"Retain($num)"
}

/**
 * The "insert" operation inserts a given string at the cursor's current
 * position in the string.
 *
 * @param chars the string to insert
 */
case class Insert(chars: String) extends OperationComponent {
  override val opType: Char = 'i'
  override def toString: String = s"""Insert(\"${chars}\")"""

}

/**
 * The "delete" operation deletes a number of characters starting
 * from the cursor's current position in the document.
 *
 * @param num the number of characters to delete.
 */
case class Delete(num: Int) extends OperationComponent {
  override val opType: Char = 'd'
  override def toString: String = s"Delete($num)"
}