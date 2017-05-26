package app

import pbmeta._

@PBSerializable
object Days extends Enumeration {
  val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
}

@PBSerializable
case class Booking(name: String, isPaid: Boolean, day: Days.Value)

object Example extends App {

  implicit class BytesToHexString(bytes: Array[Byte]) {
    def toHexString: String = bytes.toVector.map(b => "%02X".format(b)).mkString("[", " ", "]")
  }

  val booking = new Booking("Hotel", isPaid = true, Days.Wednesday)
  val bytes = booking.toByteArray
  println(s"$booking -> ${bytes.toHexString}")
  println(s"${bytes.toHexString} -> ${bytes.parseTo[Booking]}")

  val enumBytes = Days.Wednesday.toByteArray
  println(s"${Days.Wednesday} -> ${enumBytes.toHexString}")
  println(s"${enumBytes.toHexString} -> ${enumBytes.parseTo[Days.Value]}")

}
