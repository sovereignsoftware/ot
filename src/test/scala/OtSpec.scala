import org.scalatest.Matchers._
import org.scalatest._
import org.scalamock.scalatest.MockFactory
import play.api.libs.json._
import ws.kahn.ot._
import ws.kahn.ot.exceptions.{IncompatibleDeltasException}

// Is used for "should be and etc."
import org.scalamock.scalatest.MockFactory

class OtSpec extends WordSpec with MockFactory {

  "Operation" should {
    inSequence {
      "apply an operation and return the new document" in {
        val testDoc = "The quick brown fox."
        val expectedDoc = "The fast brown little fox."
        val testOpComponents = IndexedSeq[Operation](
          Retain(4),
          Insert("fast"),
          Delete(5),
          Retain(7),
          Insert("little "),
          Retain(4)
        )
        val testOp = Delta(testOpComponents, testDoc.length)

        val resultDoc = testOp.applyTo(testDoc)
        resultDoc should be(expectedDoc)
      }

      "compose two operations and then apply them to a document" in {

        val testDoc = "The cute little bunny."

        val intermediateDoc = "The caticious little cat."

        val expectedDoc = "The precious giant little cat-like stuff."

        val testOpComponentsA = IndexedSeq[Operation](
          Retain(5),
          Insert("aticious"),
          Delete(3),
          Retain(8),
          Insert("cat"),
          Delete(5),
          Retain(1)
        )

        val testOpComponentsB = IndexedSeq[Operation](
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
        val testOpA = Delta(testOpComponentsA, testDoc.length)
        val testOpB = Delta(testOpComponentsB, intermediateDoc.length)

        // Compose the two operations
        val testOp = testOpA o testOpB

        // Apply the operation against the test document
        val resultDoc = testOp.applyTo(testDoc)

//        println(s"\nInput document: $testDoc")
//        println(s"\nPerforming operation: ${testOp.toString}")
//        println(s"\nResult document: $resultDoc\n")

        resultDoc should be(expectedDoc)
      }

      "throw an exception if composing two incompatible operations" in {
        val testOpComponentsA = IndexedSeq[Operation](
          Retain(5),
          Insert("aticious"),
          Delete(3),
          Retain(8)
        )

        val testOpComponentsB = IndexedSeq[Operation](
          Retain(4),
          Delete(6),
          Insert("preci"),
          Retain(4)
        )

        val testOpA = Delta(testOpComponentsA, 16)
        val testOpB = Delta(testOpComponentsB, 25)

        val testOp = intercept[IncompatibleDeltasException] {
          testOpA.compose(testOpB)
        }
      }

      "transform two operations and successfully apply them" in {
        val startingDocument = "The cute little bunny."
        val serverEdits = Delta(IndexedSeq(
          Retain(4),
          Delete(4),
          Insert("adorable"),
          Retain(8),
          Delete(5),
          Insert("cat"),
          Delete(1),
          Insert("!!!")
        ), startingDocument.length)

        val clientEdits = Delta(IndexedSeq(
          Retain(4),
          Insert("fluffy"),
          Delete(4),
          Retain(13),
          Delete(1),
          Insert("???")
        ), startingDocument.length)

        val xfClient = serverEdits.transform(clientEdits, priority = true)
        val xfServer = clientEdits.transform(serverEdits)

        // Now apply the edits to the document and verify that they work!
        val serverText = xfClient.applyTo(serverEdits.applyTo(startingDocument))
        val clientText = xfServer.applyTo(clientEdits.applyTo(startingDocument))

        //println(s"""Starting text: $startingDocument""")
        //println(s"""Server text: $serverText""")
        //println(s"""Client text: $clientText""")

        serverText should be("The fluffyadorable little cat!!!???")
        clientText should be("The fluffyadorable little cat!!!???")
      }

      "compose operations and then transform against them" in {
        val startingDocument = "There is a cute little bunny. He runs very fast."

        val serverDocInter1 = "There is a very cute little bunny. He runs very fast."
        val serverDocInter2 = "There is a very cute little rabbit. He runs very fast."

        val finalServerDocument = "There is a very cute little rabbit. He runs quickly."

        val finalClientDocument = "There is a cute little bunny. He hops very fast."
        val finalMergedDocument = "There is a very cute little rabbit. He hops quickly."

        // Server has 3 more recent edits since the starting document
        val serverEdits = IndexedSeq(
          Delta(IndexedSeq(Retain(11), Insert("very "), Retain(37)), 48),
          Delta(IndexedSeq(Retain(28), Delete(5), Insert("rabbit"), Retain(20)), 53),
          Delta(IndexedSeq(Retain(44), Delete(9), Insert("quickly"), Retain(1)), 54)
        )

        // But the client made their edit against the starting document
        val clientEdit = Delta(IndexedSeq(Retain(33), Delete(4), Insert("hops"), Retain(11)), 48)

        // First test the edits to ensure they work...
        serverEdits(0).applyTo(startingDocument) should be(serverDocInter1)
        serverEdits(1).applyTo(serverDocInter1) should be(serverDocInter2)
        serverEdits(2).applyTo(serverDocInter2) should be(finalServerDocument)

        val timeA = System.nanoTime / 1000

        // So we'll need to compose the recent server edits into one operation
        val composedServerEdit = serverEdits.tail.foldLeft(serverEdits.head) {
          (left: Delta, right: Delta) => left compose right
        }

        val timeAfterCompose = System.nanoTime / 1000

        // And transform the client edit against this operation
        val xfClientOp = composedServerEdit.transform(clientEdit)
        val xfServerOp = clientEdit.transform(composedServerEdit)

        val timeAfterTransform = System.nanoTime / 1000

        // And then apply the transformed client edit against the final server text
        val afterServerEdits = composedServerEdit.applyTo(startingDocument)
        val afterClientEdit = clientEdit.applyTo(startingDocument)
        val afterBothEditsClient = xfClientOp.applyTo(afterServerEdits)

        // And we should also be able to apply the transformed server edits against the client's final text
        val afterBothEditsServer = xfServerOp.applyTo(afterClientEdit)

        val timeAfterApplications = System.nanoTime / 1000

        println(s"Time spent...")
        println(s"\tComposing: ${(timeAfterCompose-timeA)}")
        println(s"\tTransforming: ${timeAfterTransform-timeAfterCompose}")
        println(s"\tApplying: ${timeAfterApplications-timeAfterTransform}")
        println(s"\tTotal: ${timeAfterApplications-timeA}")

        // Assertions
        afterServerEdits should be(finalServerDocument)
        afterClientEdit should be(finalClientDocument)
        afterBothEditsClient should be(finalMergedDocument)
        afterBothEditsServer should be(finalMergedDocument)
      }

      "compose two operations with attributes" in {

        val testDoc = "The cute little bunny."

        val intermediateDoc = "The caticious little cat."

        val expectedDoc = "The precious giant little cat-like stuff."

        val testOpsA = IndexedSeq[Operation](
          Retain(5),
          Insert("aticious", Some(Map("bold" -> BooleanAttribute(true)))),
          Delete(3),
          Retain(8, Some(Map("color" -> StringAttribute("#123")))),
          Insert("cat"),
          Delete(5),
          Retain(1)
        )

        val testOpsB = IndexedSeq[Operation](
          Retain(4),
          Delete(6),
          Insert("preci"),
          Retain(4, Some(Map("bold" -> BooleanAttribute(true)))),
          Insert("giant "),
          Retain(10),
          Insert("-like stuff"),
          Retain(1)
        )

        val expectedComposedDelta = Delta(IndexedSeq[Operation](
          Retain(4),
          Delete(1),
          Insert("preci"),
          Insert("ous", Some(Map("bold" -> BooleanAttribute(true)))),
          Delete(3),
          Retain(1, Some(Map("bold" -> BooleanAttribute(true), "color" -> StringAttribute("#123")))),
          Insert("giant "),
          Retain(7, Some(Map("color" -> StringAttribute("#123")))),
          Insert("cat"),
          Insert("-like stuff"),
          Delete(5),
          Retain(1)
        ), testDoc.length)

        // Instantiate the two operations to compose
        val testDeltaA = Delta(testOpsA, testDoc.length)
        val testDeltaB = Delta(testOpsB, intermediateDoc.length)

        // Compose the two operations
        val actualComposedDelta = testDeltaA o testDeltaB

        actualComposedDelta should be(expectedComposedDelta)
      }
    }
  }
}
