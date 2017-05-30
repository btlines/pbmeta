package pbmeta

import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import scala.util.Try

trait PBFormat[A] extends PBWrites[A] with PBReads[A]

trait PBFormatImplicits {
  implicit def pbFormat[A](implicit reads: PBReads[A], writes: PBWrites[A]): PBFormat[A] =
    new PBFormat[A] {
      override def sizeOf(a: A, at: Option[Int]): Int = writes.sizeOf(a, at)
      override def write(a: A, to: CodedOutputStream, at: Option[Int]): Unit = writes.write(a, to, at)
      override def read(from: CodedInputStream): A = reads.read(from)
    }
}

object PBFormat extends PBFormatImplicits {
  def apply[A : PBFormat]: PBFormat[A] = implicitly
}
