package pbmeta

import org.scalatest.{ Matchers, WordSpecLike }

class PBReadsSpec extends WordSpecLike with Matchers {
  "PBReads" should {
    "read a Boolean from Protobuf" in {
      @PBSerializable case class BooleanMessage(value: Option[Boolean])
      val bytes = Array[Byte](8, 1)
      bytes.pbTo[BooleanMessage] shouldBe BooleanMessage(Some(true))
    }
    "read an Int from Protobuf" in {
      @PBSerializable case class IntMessage(value: Option[Int])
      val bytes = Array[Byte](8, 5)
      bytes.pbTo[IntMessage] shouldBe IntMessage(Some(5))
    }
    "read a Long from Protobuf" in {
      @PBSerializable case class LongMessage(value: Option[Long])
      val bytes = Array[Byte](8, -128, -128, -128, -128, 8)
      bytes.pbTo[LongMessage] shouldBe LongMessage(Some(Int.MaxValue.toLong + 1))
    }
    "read a Float from Protobuf" in {
      @PBSerializable case class FloatMessage(value: Option[Float])
      val bytes = Array[Byte](13, -51, -52, 76, 62)
      bytes.pbTo[FloatMessage] shouldBe FloatMessage(Some(0.2F))
    }
    "read a Double from Protobuf" in {
      @PBSerializable case class DoubleMessage(value: Option[Double])
      val bytes = Array[Byte](9, -107, 100, 121, -31, 127, -3, -75, 61)
      bytes.pbTo[DoubleMessage] shouldBe DoubleMessage(Some(0.00000000002D))
    }
    "read a String from Protobuf" in {
      @PBSerializable case class StringMessage(value: Option[String])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[StringMessage] shouldBe StringMessage(Some("Hello"))
    }
    "read bytes from Protobuf" in {
      @PBSerializable case class BytesMessage(value: Option[Array[Byte]])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[BytesMessage].value.get shouldBe Array[Byte](72, 101, 108, 108, 111)
    }
    "read an enumeration from Protobuf" ignore {
      @PBSerializable object Grade extends Enumeration {
        val GradeA, GradeB = Value
      }
      val bytesA = Array[Byte](8, 0)
      val bytesB = Array[Byte](8, 1)
      @PBSerializable case class GradeMessage(value: Option[Grade.Value])
      bytesA.pbTo[GradeMessage] shouldBe GradeMessage(Some(Grade.GradeA))
      bytesB.pbTo[GradeMessage] shouldBe GradeMessage(Some(Grade.GradeB))
    }
    "read a required field from Protobuf" in {
      @PBSerializable case class RequiredMessage(value: Int)
      val bytes = Array[Byte](8 , 5)
      bytes.pbTo[RequiredMessage] shouldBe RequiredMessage(5)
    }
    "read an empty message from Protobuf" in {
      @PBSerializable case class EmptyMessage()
      val bytes = Array[Byte]()
      bytes.pbTo[EmptyMessage] shouldBe EmptyMessage()
    }
    "read a multi-field message from Protobuf" in {
      @PBSerializable case class MultiMessage(text: Option[String], number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
    "read a message with missing field from Protobuf" in {
      @PBSerializable case class MissingMessage(text: Option[String], number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[MissingMessage] shouldBe MissingMessage(Some("Hello"), None)
    }
    "read a message with repeated field from Protobuf" in {
      @PBSerializable case class RepeatedMessage(values: List[Int])
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      bytes.pbTo[RepeatedMessage] shouldBe RepeatedMessage(1 :: 2 :: 3 :: 4 :: Nil)
    }
    "read a Map from Protobuf" in {
      @PBSerializable case class MapMessage(values: Map[Int, String])
      val bytes = Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
      bytes.pbTo[MapMessage] shouldBe MapMessage(Map(1 -> "one", 2 -> "two"))
    }
    "read a nested message from Protobuf" in {
      @PBSerializable case class InnerMessage(value: Option[Int])
      @PBSerializable case class OuterMessage(text: Option[String], inner: Option[InnerMessage])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
      bytes.pbTo[OuterMessage] shouldBe OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
    }
  }
}
