package pbmeta

import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import scala.util.Try

trait PBWrites[A] {
  def write(a: A, to: CodedOutputStream, at: Option[Int] = None): Unit
}
trait PBReads[A] {
  def read(from: CodedInputStream): A
}
trait PBFormat[A] extends PBWrites[A] with PBReads[A]

trait PBWritableImplicits {
  implicit object PBWritesBool extends PBWrites[Boolean] {
    override def write(a: Boolean, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeBool(i, a)
        case None    => to.writeBoolNoTag(a)
      }
  }
  implicit object PBWritesInt extends PBWrites[Int] {
    override def write(a: Int, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeInt32(i, a)
        case None    => to.writeInt32NoTag(a)
      }
  }
  implicit object PBWritesLong extends PBWrites[Long] {
    override def write(a: Long, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeInt64(i, a)
        case None    => to.writeInt64NoTag(a)
      }
  }
  implicit object PBWritesFloat extends PBWrites[Float] {
    override def write(a: Float, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeFloat(i, a)
        case None    => to.writeFloatNoTag(a)
      }
  }
  implicit object PBWritesDouble extends PBWrites[Double] {
    override def write(a: Double, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeDouble(i, a)
        case None    => to.writeDoubleNoTag(a)
      }
  }
  implicit object PBWritesString extends PBWrites[String] {
    override def write(a: String, to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeString(i, a)
        case None    => to.writeStringNoTag(a)
      }
  }
  implicit object PBWritesBytes extends PBWrites[Array[Byte]] {
    override def write(a: Array[Byte], to: CodedOutputStream, at: Option[Int]): Unit =
      at match {
        case Some(i) => to.writeByteArray(i, a)
        case None    => to.writeByteArrayNoTag(a)
      }
  }
  implicit def pbWritesEnum[E <: Enumeration]: PBWrites[E#Value] =
    new PBWrites[E#Value] {
      override def write(a: E#Value, to: CodedOutputStream, at: Option[Int]): Unit =
        PBWritesInt.write(a.id, to, at)
    }
}

trait PBReadsImplicits {
  implicit object PBReadsBool extends PBReads[Boolean] {
    override def read(from: CodedInputStream): Boolean =
      from.readBool()
  }
  implicit object PBReadsInt extends PBReads[Int] {
    override def read(from: CodedInputStream): Int =
      from.readInt32()
  }
  implicit object PBReadsLong extends PBReads[Long] {
    override def read(from: CodedInputStream): Long =
      from.readInt64()
  }
  implicit object PBReadsFloat extends PBReads[Float] {
    override def read(from: CodedInputStream): Float =
      from.readFloat()
  }
  implicit object PBReadsDouble extends PBReads[Double] {
    override def read(from: CodedInputStream): Double =
      from.readDouble()
  }
  implicit object PBReadsString extends PBReads[String] {
    override def read(from: CodedInputStream): String =
      from.readString()
  }
  implicit object PBReadsBytes extends PBReads[Array[Byte]] {
    override def read(from: CodedInputStream): Array[Byte] =
      from.readByteArray()
  }
  def enum[E <: Enumeration](enum: E): PBReads[E#Value] =
    new PBReads[E#Value] {
      override def read(from: CodedInputStream): E#Value =
        enum(PBReadsInt.read(from))
    }
}

trait PBFormatImplicits {
  implicit def pbFormat[A](implicit reads: PBReads[A], writes: PBWrites[A]): PBFormat[A] =
    new PBFormat[A] {
      override def write(a: A, to: CodedOutputStream, at: Option[Int]): Unit =
        writes.write(a, to, at)
      override def read(from: CodedInputStream): A =
        reads.read(from)
    }
}

object PBWrites extends PBWritableImplicits {
  def apply[A : PBWrites]: PBWrites[A] = implicitly
}

object PBReads extends PBReadsImplicits {
  def apply[A : PBReads]: PBReads[A] = implicitly
}

object PBFormat extends PBFormatImplicits {
  def apply[A : PBFormat]: PBFormat[A] = implicitly
}
