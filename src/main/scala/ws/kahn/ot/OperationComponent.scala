package ws.kahn.ot

sealed trait OperationComponent {
  val opType: Char
}

/**
 * The "retain" operation moves the cursor along the document by
 * a number of characters, effectively "skipping" over them.
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