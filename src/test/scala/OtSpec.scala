import org.scalatest.Matchers._
import org.scalatest._
import org.scalamock.scalatest.MockFactory
import play.api.libs.json._
import ws.kahn.ot._
import ws.kahn.ot.exceptions.{IncompatibleDeltasException}

// Is used for "should be and etc."
import org.scalamock.scalatest.MockFactory

class OtSpec extends WordSpec with MockFactory {

  "Delta" should {
    "compose two operations and then apply them to a document" in {

      val testDoc = Delta(IndexedSeq(InsertText("The cute little bunny.")))
      val expectedDelta = Delta(IndexedSeq(InsertText("The precious giant little "), InsertCode(0), InsertText("cat-like stuff.")))

      val testDeltaA = Delta(IndexedSeq[Operation](
        Retain(5),
        InsertText("aticious"),
        Delete(3),
        Retain(8),
        InsertCode(0),
        InsertText("cat"),
        Delete(5),
        Retain(1)
      ))

      val testDeltaB = Delta(IndexedSeq[Operation](
        Retain(4),
        Delete(6),
        InsertText("preci"),
        Retain(4),
        InsertText("giant "),
        Retain(11),
        InsertText("-like stuff"),
        Retain(1)
      ))

      // Compose the two operations
      val composedDelta = testDeltaA.compose(testDeltaB)
      val resultDelta = testDoc.compose(composedDelta)

      resultDelta should be(expectedDelta)
    }

    "throw an exception if composing two incompatible operations" in {
      val testOpComponentsA = IndexedSeq[Operation](
        Retain(5),
        InsertText("aticious"),
        Delete(3),
        Retain(8)
      )

      val testOpComponentsB = IndexedSeq[Operation](
        Retain(4),
        Delete(6),
        InsertText("preci"),
        Retain(4)
      )

      val testOpA = Delta(testOpComponentsA)
      val testOpB = Delta(testOpComponentsB)

      val testOp = intercept[IncompatibleDeltasException] {
        testOpA.compose(testOpB)
      }
    }

    "transform two operations and successfully apply them" in {
      val startingDelta = Delta(IndexedSeq(InsertText("The cute little bunny.")))
      val expectedDelta = Delta(IndexedSeq(InsertText("The fluffyadorable"), InsertCode(0), InsertText(" little cat!!!???")))

      val serverEdits = Delta(IndexedSeq(
        Retain(4),
        Delete(4),
        InsertText("adorable"),
        InsertCode(0),
        Retain(8),
        Delete(5),
        InsertText("cat"),
        Delete(1),
        InsertText("!!!")
      ))

      val clientEdits = Delta(IndexedSeq(
        Retain(4),
        InsertText("fluffy"),
        Delete(4),
        Retain(13),
        Delete(1),
        InsertText("???")
      ))

      val xfClient = serverEdits.transform(clientEdits, priority = true)
      val xfServer = clientEdits.transform(serverEdits)

      // Now apply the edits to the document and verify that they work!
      val serverDelta = startingDelta.compose(serverEdits).compose(xfClient)
      val clientDelta = startingDelta.compose(clientEdits).compose(xfServer)

      serverDelta should be(expectedDelta)
      clientDelta should be(expectedDelta)
    }

    "compose operations and then transform against them" in {
      val startingDocument = Delta(IndexedSeq(InsertText("There is a cute little bunny. He runs very fast.")))
      val finalServerDocument = Delta(IndexedSeq(InsertText("There is a very cute little rabbit. He runs quickly.")))

      val finalClientDocument = Delta(IndexedSeq(InsertText("There is a cute little bunny. He hops very fast.")))
      val finalMergedDocument = Delta(IndexedSeq(InsertText("There is a very cute little rabbit. He hops quickly.")))

      // Server has 3 more recent edits since the starting document
      val serverEdits = IndexedSeq(
        Delta(IndexedSeq(Retain(11), InsertText("very "), Retain(37))),
        Delta(IndexedSeq(Retain(28), Delete(5), InsertText("rabbit"), Retain(20))),
        Delta(IndexedSeq(Retain(44), Delete(9), InsertText("quickly"), Retain(1)))
      )

      // But the client made their edit against the starting document
      val clientEdit = Delta(IndexedSeq(Retain(33), Delete(4), InsertText("hops"), Retain(11)))

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
      val afterServerEdits = startingDocument.compose(composedServerEdit)
      val afterClientEdit = startingDocument.compose(clientEdit)
      val afterBothEditsClient = afterServerEdits.compose(xfClientOp)

      // And we should also be able to apply the transformed server edits against the client's final text
      val afterBothEditsServer = afterClientEdit.compose(xfServerOp)

      val timeAfterApplications = System.nanoTime / 1000

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
        InsertText("aticious", Some(Map("bold" -> BooleanAttribute(true)))),
        Delete(3),
        Retain(8, Some(Map("color" -> StringAttribute("#123")))),
        InsertText("cat"),
        Delete(5),
        Retain(1)
      )

      val testOpsB = IndexedSeq[Operation](
        Retain(4),
        Delete(6),
        InsertText("preci"),
        Retain(4, Some(Map("bold" -> BooleanAttribute(true)))),
        InsertText("giant "),
        Retain(10),
        InsertText("-like stuff"),
        Retain(1)
      )

      val expectedComposedDelta = Delta(IndexedSeq[Operation](
        Retain(4),
        Delete(1),
        InsertText("preci"),
        InsertText("ous", Some(Map("bold" -> BooleanAttribute(true)))),
        Delete(3),
        Retain(1, Some(Map("bold" -> BooleanAttribute(true), "color" -> StringAttribute("#123")))),
        InsertText("giant "),
        Retain(7, Some(Map("color" -> StringAttribute("#123")))),
        InsertText("cat-like stuff"),
        Delete(5),
        Retain(1)
      ))

      // Instantiate the two operations to compose
      val testDeltaA = Delta(testOpsA)
      val testDeltaB = Delta(testOpsB)

      // Compose the two operations
      val actualComposedDelta = testDeltaA o testDeltaB

      actualComposedDelta should be(expectedComposedDelta)
    }

    "Unserialize from a JSON string" in {
      val jsonStr =
        Json.parse("""
          |{
          |  "ops": [
          |    {"retain": 10},
          |    {"insert": "cat", "attributes": { "bold": true }},
          |    {"retain": 5, "attributes": {"bold": true}},
          |    {"delete": 2},
          |    {"retain": 3, "attributes": {"bold": null, "italic": null}}
          |  ]
          |}
        """.stripMargin)

      val expectedDelta = Delta(IndexedSeq(
        Retain(10),
        InsertText("cat", Some(Map("bold" -> BooleanAttribute(true)))),
        Retain(5, Some(Map("bold" -> BooleanAttribute(true)))),
        Delete(2),
        Retain(3, Some(Map("bold" -> NullAttribute(), "italic" -> NullAttribute())))
      ))

      val result = Delta.reads.reads(jsonStr)
      result should be(JsSuccess(expectedDelta))
    }

    "Serialize to a JSON string" in {

    }
  }
}
