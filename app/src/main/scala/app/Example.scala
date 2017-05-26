package app

import pbmeta._

@PBSerializable
object Days extends Enumeration {
  val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
}

@PBSerializable
case class Booking(name: String, isPaid: Boolean)
@PBSerializable case class OneDay(day: Option[Days.Value])

object Example extends App {

  implicit class BytesToHexString(bytes: Array[Byte]) {
    def toHexString: String = bytes.toVector.map(b => "%02X".format(b)).mkString("[", " ", "]")
  }

  val booking = new Booking("Hotel", isPaid = true)
  val bytes = booking.toPB
  println(s"$booking -> ${bytes.toHexString}")
  println(s"${bytes.toHexString} -> ${bytes.pbTo[Booking]}")

  val enumBytes = Days.Wednesday.toPB
  println(s"${Days.Wednesday} -> ${enumBytes.toHexString}")
  println(s"${enumBytes.toHexString} -> ${enumBytes.pbTo[Days.Value]}")

  val oneDay = OneDay(Some(Days.Friday))
  val dayBytes = oneDay.toPB
  println(s"${oneDay} -> ${dayBytes.toHexString}")
  println(s"${dayBytes.toHexString} -> ${dayBytes.pbTo[OneDay]}")

}
