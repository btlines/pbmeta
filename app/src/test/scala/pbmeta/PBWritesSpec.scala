package pbmeta

import org.scalatest.{ Matchers, WordSpec }

object PBWritesSpec {
  @PBSerializable object Grade extends Enumeration {
    val GradeA, GradeB = Value
  }
  @PBSerializable case class EnumMessage(value: Grade.Value)
}

class PBWritesSpec extends WordSpec with Matchers {
  "PBWrites" should {
    "write a Boolean to Protobuf" in {
      @PBSerializable case class BooleanMessage(value: Option[Boolean])
      val message = BooleanMessage(Some(true))
      message.toPB shouldBe Array[Byte](8, 1)
    }
    "write an Int to Protobuf" in {
      @PBSerializable case class IntMessage(value: Option[Int])
      val message = IntMessage(Some(5))
      message.toPB shouldBe Array[Byte](8, 5)
    }
    "write a Long to Protobuf" in {
      @PBSerializable case class LongMessage(value: Option[Long])
      val message = LongMessage(Some(Int.MaxValue.toLong + 1))
      message.toPB shouldBe Array[Byte](8, -128, -128, -128, -128, 8)
    }
    "write a Float to Protobuf" in {
      @PBSerializable case class FloatMessage(value: Option[Float])
      val message = FloatMessage(Some(0.2F))
      message.toPB shouldBe Array[Byte](13, -51, -52, 76, 62)
    }
    "write a Double to Protobuf" in {
      @PBSerializable case class DoubleMessage(value: Option[Double])
      val message = DoubleMessage(Some(0.00000000002D))
      message.toPB shouldBe Array[Byte](9, -107, 100, 121, -31, 127, -3, -75, 61)
    }
    "write a String to Protobuf" in {
      @PBSerializable case class StringMessage(value: Option[String])
      val message = StringMessage(Some("Hello"))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write bytes to Protobuf" in {
      @PBSerializable case class BytesMessage(value: Array[Byte])
      val message = BytesMessage(Array[Byte](8, 4))
      message.toPB shouldBe Array[Byte](10, 2, 8, 4)
    }
    "write an enumeration to Protobuf" in {
      import PBWritesSpec._
      val message = EnumMessage(Grade.GradeB)
      message.toPB shouldBe Array[Byte](8, 1)
    }
    "write a required field to Protobuf" in {
      @PBSerializable case class RequiredMessage(value: Int)
      val message = RequiredMessage(5)
      message.toPB shouldBe Array[Byte](8 , 5)
    }
    "write an empty message to Protobuf" in {
      @PBSerializable case class EmptyMessage()
      val message = EmptyMessage()
      message.toPB shouldBe Array[Byte]()
    }
    "write a multi-field message to Protobuf" in {
      @PBSerializable case class MultiMessage(text: Option[String], number: Option[Int])
      val message = MultiMessage(Some("Hello"), Some(3))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
    }
    "write a message with missing field to Protobuf" in {
      @PBSerializable case class MissingMessage(text: Option[String], number: Option[Int])
      val message = MissingMessage(Some("Hello"), None)
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write a message with repeated field to Protobuf" in {
      @PBSerializable case class RepeatedMessage(values: List[Int])
      val message = RepeatedMessage(1 :: 2 :: 3 :: 4 :: Nil)
      message.toPB shouldBe Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
    }
    "write a Map to Protobuf" in {
      @PBSerializable case class MapMessage(values: Map[Int, String])
      val message = MapMessage(Map(1 -> "one", 2 -> "two"))
      message.toPB shouldBe Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
    }
    "write a nested message to Protobuf" in {
      @PBSerializable case class InnerMessage(value: Option[Int])
      @PBSerializable case class OuterMessage(text: Option[String], inner: Option[InnerMessage])
      val message = OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
    }
    "write a multi-field message to Protobuf with specific indices" in {
      @PBSerializable case class MultiMessage(@Pos(2) text: Option[String], number: Option[Int])
      val message = MultiMessage(Some("Hello"), Some(3))
      message.toPB shouldBe Array[Byte](18, 5, 72, 101, 108, 108, 111, 24, 3)
    }
  }
}
