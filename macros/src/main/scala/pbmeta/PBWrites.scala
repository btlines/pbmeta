package pbmeta

import com.google.protobuf.{CodedOutputStream, WireFormat}

trait PBWrites[A] {
  def write(a: A, to: CodedOutputStream, at: Option[Int] = None): Unit
  def sizeOf(a: A, at: Option[Int] = None): Int
}

trait PBWritableImplicits {
  implicit object PBWritesBool extends PBWrites[Boolean] {
    override def write(a: Boolean, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeBool(i, a)
        case None    => to.writeBoolNoTag(a)
      }
    override def sizeOf(a: Boolean, at: Option[Int]): Int =
      at match {
        case Some(i) => CodedOutputStream.computeBoolSize(i, a)
        case None    => CodedOutputStream.computeBoolSizeNoTag(a)
      }
  }
  implicit object PBWritesInt extends PBWrites[Int] {
    override def write(a: Int, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeInt32(i, a)
        case None => to.writeInt32NoTag(a)
      }
    override def sizeOf(a: Int, at: Option[Int]): Int =
      at match {
        case Some(i) => CodedOutputStream.computeInt32Size(i, a)
        case None    => CodedOutputStream.computeInt32SizeNoTag(a)
      }
  }
  implicit object PBWritesLong extends PBWrites[Long] {
    override def write(a: Long, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeInt64(i, a)
        case None    => to.writeInt64NoTag(a)
      }
    override def sizeOf(a: Long, at: Option[Int]): Int =
      at match {
        case Some(i) => CodedOutputStream.computeInt64Size(i, a)
        case None    => CodedOutputStream.computeInt64SizeNoTag(a)
      }
  }
  implicit object PBWritesFloat extends PBWrites[Float] {
    override def write(a: Float, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeFloat(i, a)
        case None    => to.writeFloatNoTag(a)
      }
    override def sizeOf(a: Float, at: Option[Int]): Int =
      at match {
        case Some(i) => CodedOutputStream.computeFloatSize(i, a)
        case None    => CodedOutputStream.computeFloatSizeNoTag(a)
      }
  }
  implicit object PBWritesDouble extends PBWrites[Double] {
    override def write(a: Double, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeDouble(i, a)
        case None    => to.writeDoubleNoTag(a)
      }
    override def sizeOf(a: Double, at: Option[Int]): Int =
      at match {
        case Some(i) => CodedOutputStream.computeDoubleSize(i, a)
        case None    => CodedOutputStream.computeDoubleSizeNoTag(a)
      }
  }
  implicit object PBWritesString extends PBWrites[String] {
    override def write(a: String, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeString(i, a)
        case None    => to.writeStringNoTag(a)
      }
    override def sizeOf(a: String, at: Option[Int]): Int =
      at match {
        case Some(i) => CodedOutputStream.computeStringSize(i, a)
        case None    => CodedOutputStream.computeStringSizeNoTag(a)
      }
  }
  implicit object PBWritesBytes extends PBWrites[Array[Byte]] {
    override def write(a: Array[Byte], to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeByteArray(i, a)
        case None    => to.writeByteArrayNoTag(a)
      }
    override def sizeOf(a: Array[Byte], at: Option[Int]): Int =
      at match {
        case Some(i) => CodedOutputStream.computeByteArraySize(i, a)
        case None    => CodedOutputStream.computeByteArraySizeNoTag(a)
      }
  }
  implicit def pbWritesKV[K : PBWrites, V : PBWrites]: PBWrites[(K, V)] =
    new PBWrites[(K, V)] {
      override def write(a: (K, V), to: CodedOutputStream, at: Option[Int]): Unit = {
        at.foreach(i => to.writeTag(i, WireFormat.WIRETYPE_LENGTH_DELIMITED))
        to.writeUInt32NoTag(sizeOf(a))
        PBWrites[K].write(a._1, to, Some(1))
        PBWrites[V].write(a._2, to, Some(2))
      }
      override def sizeOf(a: (K, V), at: Option[Int]): Int =
        at.map(CodedOutputStream.computeTagSize).getOrElse(0) +
        PBWrites[K].sizeOf(a._1, Some(1)) +
        PBWrites[V].sizeOf(a._2, Some(2))
    }
  implicit def pbWritesEnum[E <: Enumeration]: PBWrites[E#Value] =
    new PBWrites[E#Value] {
      override def write(a: E#Value, to: CodedOutputStream, at: Option[Int]): Unit =
        PBWritesInt.write(a.id, to, at)
      override def sizeOf(a: E#Value, at: Option[Int]): Int =
        at match {
          case Some(i) => CodedOutputStream.computeInt32Size(i, a.id)
          case None    => CodedOutputStream.computeInt32SizeNoTag(a.id)
        }
    }
}

object PBWrites extends PBWritableImplicits {
  def apply[A : PBWrites]: PBWrites[A] = implicitly
}