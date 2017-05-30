package pbmeta

import org.scalatest.{ Matchers, WordSpec }

class PBFormatSpec extends WordSpec with Matchers {

  "PBFormat" should {
    "be derived automatically from PBReads and PBWrites" in {
      @PBSerializable case class BooleanMessage(value: Boolean)
      PBReads[BooleanMessage]
      PBWrites[BooleanMessage]
      PBFormat[BooleanMessage]
    }
  }

}
