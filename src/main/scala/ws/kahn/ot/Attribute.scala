package ws.kahn.ot

import play.api.libs.json._

sealed trait Attribute {

}

case class NumberAttribute(value: Double) extends Attribute
case class StringAttribute(value: String) extends Attribute
case class BooleanAttribute(value: Boolean) extends Attribute
class NullAttribute extends Attribute

object Attribute {
  type AttributeList = Option[Map[String, Attribute]]

  implicit val mapReads = new Reads[Map[String, Attribute]] {
    def reads(json: JsValue): JsResult[Map[String, Attribute]] = {
      json match {
        case jsobj: JsObject => {
          JsSuccess {
            jsobj.value.map { case (k, v) =>
              (k, v match {
                case JsNull => NullAttribute()
                case num: JsNumber => NumberAttribute(num.as[Double])
                case str: JsString => StringAttribute(str.as[String])
                case bool: JsBoolean => BooleanAttribute(bool.as[Boolean])
                case _ => NullAttribute()
              })
            }.toMap[String, Attribute]
          }
        }
        case _ => JsError("Attributes must be a JsObject")
      }
    }
  }

  implicit val mapWrites = new Writes[Map[String, Attribute]] {
    def writes(attrs: Map[String, Attribute]): JsValue = {
      val mapped = attrs.map { case (k,v) =>
        (k, v match {
          case num: NumberAttribute => JsNumber(num.value)
          case str: StringAttribute => JsString(str.value)
          case bool: BooleanAttribute => JsBoolean(bool.value)
          case nul: NullAttribute => JsNull
        })
      }
      JsObject(mapped.toSeq)
    }
  }

  /**
   * Merges two lists of attributes, allowing the second list to overwrite values from
   * the first.
   */
  def compose(firstO: AttributeList, secondO: AttributeList, keepNull: Boolean = false): AttributeList = {
    (firstO, secondO) match {
      case (None, None) => None
      case (Some(first), None) => Some(first)
      case (None, Some(second)) => Some(second)
      case (Some(first), Some(second)) => {
        val composed = first ++ second

        if (keepNull) {
          Some(composed)
        }
        else {
          filterNull(Some(composed))
        }
      }
    }

  }

  /**
   * Merges two lists of attributes, where values from the second are only applied if they
   * don't already appear in the first.
   */
  def transform(firstO: AttributeList, secondO: AttributeList, priority: Boolean = false): AttributeList = {
    (firstO, secondO) match {
      case (None, None) => None
      case (Some(first), None) => Some(first)
      case (None, Some(second)) => Some(second)
      case (Some(first), Some(second)) => {
        Some {
          if (!priority) { second }
          else {
            second ++ first
          }
        }
      }
    }
  }

  /**
   * Get the disjunction between two maps of attributes.
   */
  def diff(firstO: AttributeList, secondO: AttributeList): AttributeList = {
    (firstO, secondO) match {
      case (None, None) => None
      case (Some(first), None) => Some(first)
      case (None, Some(second)) => Some(second)
      case (Some(first), Some(second)) => {
        Some {
          first.filterNot({case (k,v) => second.contains(k)}) ++
            second.filterNot({case (k,v) => first.contains(k)})
        }
      }
    }
  }

  /**
   * Removes "NullAttribute" values from the list.
   *
   * @param attrs
   * @return
   */
  def filterNull(attrs: AttributeList): AttributeList = {
    attrs match {
      case None => None
      case Some(list) => Some(list.filterNot { case (key, value) => value match {
        case value: NullAttribute => true
        case _ => false
      }})
    }
  }
}

object NumberAttribute {
  implicit val reads = new Reads[NumberAttribute] {
    def reads(json: JsValue) = {
      json.asOpt[Double] match {
        case Some(value) => JsSuccess(NumberAttribute(value))
        case None => JsError("Not a valid JsNumber")
      }
    }
  }
  implicit val writes = new Writes[NumberAttribute] {
    def writes(number: NumberAttribute): JsValue = {
      JsNumber(number.value)
    }
  }
}

object StringAttribute {
  implicit val reads = new Reads[StringAttribute] {
    def reads(json: JsValue) = {
      json.asOpt[String] match {
        case Some(value) => JsSuccess(StringAttribute(value))
        case None => JsError("Not a valid JsNumber")
      }
    }
  }
  implicit val writes = new Writes[StringAttribute] {
    def writes(string: StringAttribute): JsValue = {
      JsString(string.value)
    }
  }
}

object BooleanAttribute {
  implicit val reads = new Reads[BooleanAttribute] {
    def reads(json: JsValue) = {
      json.asOpt[Boolean] match {
        case Some(value) => JsSuccess(BooleanAttribute(value))
        case None => JsError("Not a valid JsNumber")
      }
    }
  }
  implicit val writes = new Writes[BooleanAttribute] {
    def writes(bool: BooleanAttribute): JsValue = {
      JsBoolean(bool.value)
    }
  }
}

object NullAttribute {
  def apply() = new NullAttribute()

  implicit val reads = new Reads[NullAttribute] {
    def reads(json: JsValue) = {
      json match {
        case JsNull => JsSuccess(new NullAttribute)
        case _ => JsError("Not a valid JsNull")
      }
    }
  }
  implicit val writes = new Writes[NullAttribute] {
    def writes(nullAttr: NullAttribute): JsValue = JsNull
  }
}