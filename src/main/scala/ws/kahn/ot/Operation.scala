package ws.kahn.ot

import play.api.libs.json._
import play.api.libs.functional.syntax._

sealed trait Operation {
  val opType: Char

  def length: Int
}

sealed trait AttributedOperation extends Operation {
  val attributes: Option[JsObject]
}

object Operation {

  object Types {
    val Retain = 0
    val Insert = 1
    val Delete = 2
  }

  implicit val opReads = new Reads[Operation] {
    def reads(json: JsValue) = {
      {(json \ "retain").asOpt[Int].map { num => Retain.retainReads.reads(json) } orElse
        (json \ "insert").asOpt[Int].map { num => Insert.insertReads.reads(json) } orElse
          (json \ "retain").asOpt[Int].map { num => Delete.deleteReads.reads(json) }} getOrElse
            JsError("Error reading operation")
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
case class Retain(num: Int, attributes: Option[JsObject] = None) extends AttributedOperation {
  override val opType: Char = 'r'

  override def toString: String = s"Retain($num, ${attributes.toString()})"

  override def length: Int = num
}
object Retain {
  implicit val retainReads = (
    (__ \ "retain").read[Int] and
      (__ \ "attributes").readNullable[JsObject]
    )(Retain.apply _)

  implicit val retainWrites = (
      (__ \ "retain").write[Int] and
        (__ \ "attributes").writeNullable[JsObject]
    )(unlift(Retain.unapply))
}

/**
 * The "insert" operation inserts a given string at the cursor's current
 * position in the string.
 *
 * @param chars the string to insert
 */
case class Insert(chars: String, attributes: Option[JsObject] = None) extends AttributedOperation {
  override val opType: Char = 'i'

  override def toString: String = s"""Insert(\"${chars}\", ${attributes.toString()})"""

  override def length: Int = chars.length
}

object Insert {
  implicit val insertReads = (
    (__ \ "insert").read[String] and
      (__ \ "attributes").readNullable[JsObject]
    )(Insert.apply _)

  implicit val insertWrites = (
    (__ \ "insert").write[String] and
      (__ \ "attributes").writeNullable[JsObject]
    )(unlift(Insert.unapply))
}

/**
 * The "delete" operation deletes a number of characters starting
 * from the cursor's current position in the document.
 *
 * @param num the number of characters to delete.
 */
case class Delete(num: Int) extends Operation {
  override val opType: Char = 'd'

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
