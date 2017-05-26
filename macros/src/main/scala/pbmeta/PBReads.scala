package pbmeta

import com.google.protobuf.CodedInputStream

trait PBReads[A] {
  def read(from: CodedInputStream): A
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
  implicit def pbReadsPair[K : PBReads, V : PBReads]: PBReads[(K, V)] =
    new PBReads[(K, V)] {
      override def read(from: CodedInputStream): (K, V) = {
        var k: List[K] = Nil
        var v: List[V] = Nil
        while (k.isEmpty || v.isEmpty) {
          from.readTag match {
            case tag if (tag >> 3) == 1 => k ::= PBReads[K].read(from)
            case tag if (tag >> 3) == 2 => v ::= PBReads[V].read(from)
          }
        }
        (k.head, v.head)
      }
    }
  def enum[E <: Enumeration](enum: E): PBReads[E#Value] =
    new PBReads[E#Value] {
      override def read(from: CodedInputStream): E#Value =
        enum(PBReadsInt.read(from))
    }
}

object PBReads extends PBReadsImplicits {
  def apply[A : PBReads]: PBReads[A] = implicitly
}
