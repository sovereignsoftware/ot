package ws.kahn.ot.exceptions

import ws.kahn.ot.Delta

case class IncompatibleDeltasException(leftDelta: Delta, rightDelta: Delta) extends Exception
