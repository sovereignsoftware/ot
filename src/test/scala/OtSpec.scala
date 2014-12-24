import org.scalatest.Matchers._
import org.scalatest._
import org.scalamock.scalatest.MockFactory
import ws.kahn.ot._
import ws.kahn.ot.exceptions.IncompatibleOperationsException

// Is used for "should be and etc."
import org.scalamock.scalatest.MockFactory

class OtSpec extends WordSpec with MockFactory {

  "Operation" should {
    inSequence {
      "apply an operation and return the new document" in {
        val testDoc = "The quick brown fox."
        val expectedDoc = "The fast brown little fox."
        val testOpComponents = IndexedSeq[OperationComponent](
          Retain(4),
          Insert("fast"),
          Delete(5),
          Retain(7),
          Insert("little "),
          Retain(4)
        )
        val testOp = Operation(testOpComponents, testDoc.length)

        val resultDoc = testOp.applyTo(testDoc)
        resultDoc should be(expectedDoc)
      }

      "compose two operations and then apply them to a document" in {

        val testDoc = "The cute little bunny."

        val intermediateDoc = "The caticious little cat."

        val expectedDoc = "The precious giant little cat-like stuff."

        val testOpComponentsA = IndexedSeq[OperationComponent](
          Retain(5),
          Insert("aticious"),
          Delete(3),
          Retain(8),
          Insert("cat"),
          Delete(5),
          Retain(1)
        )

        val testOpComponentsB = IndexedSeq[OperationComponent](
          Retain(4),
          Delete(6),
          Insert("preci"),
          Retain(4),
          Insert("giant "),
          Retain(10),
          Insert("-like stuff"),
          Retain(1)
        )

        // Instantiate the two operations to compose
        val testOpA = Operation(testOpComponentsA, testDoc.length)
        val testOpB = Operation(testOpComponentsB, intermediateDoc.length)

        // Compose the two operations
        val testOp = testOpA ++ testOpB

        // Apply the operation against the test document
        val resultDoc = testOp.applyTo(testDoc)

        println(s"\nInput document: $testDoc")
        println(s"\nPerforming operation: ${testOp.toString}")
        println(s"\nResult document: $resultDoc\n")

        resultDoc should be(expectedDoc)
      }

      "throw an exception if composing two incompatible operations" in {
        val testOpComponentsA = IndexedSeq[OperationComponent](
          Retain(5),
          Insert("aticious"),
          Delete(3),
          Retain(8)
        )

        val testOpComponentsB = IndexedSeq[OperationComponent](
          Retain(4),
          Delete(6),
          Insert("preci"),
          Retain(4)
        )

        val testOpA = Operation(testOpComponentsA, 16)
        val testOpB = Operation(testOpComponentsB, 25)

        val testOp = intercept[IncompatibleOperationsException] {
          testOpA ++ testOpB
        }
      }

      "transform two operations and successfully apply them" in {
        val startingDocument = "The cute little bunny."
        val serverEdits = Operation(IndexedSeq(
          Retain(4),
          Delete(4),
          Insert("adorable"),
          Retain(8),
          Delete(5),
          Insert("cat"),
          Delete(1),
          Insert("!!!")
        ), startingDocument.length)

        val clientEdits = Operation(IndexedSeq(
          Retain(4),
          Insert("fluffy"),
          Delete(4),
          Retain(13),
          Delete(1),
          Insert("???")
        ), startingDocument.length)

        val (xfServer, xfClient) = Operation.transform(serverEdits, clientEdits)

        // Now apply the edits to the document and verify that they work!
        val serverText = xfClient.applyTo(serverEdits.applyTo(startingDocument))
        val clientText = xfServer.applyTo(clientEdits.applyTo(startingDocument))

        println(s"""Starting text: $startingDocument""")
        println(s"""Server text: $serverText""")
        println(s"""Client text: $clientText""")

        serverText should be("The fluffyadorable little cat!!!???")
        clientText should be("The fluffyadorable little cat!!!???")
      }
    }
  }
}
