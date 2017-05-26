package object pbmeta {

  implicit class PBWriteOps[A : PBWrites](a: A) {
    def toByteArray: Array[Byte] = {
      val out = new java.io.ByteArrayOutputStream()
      val to = com.google.protobuf.CodedOutputStream.newInstance(out)
      PBWrites[A].write(a, to)
      to.flush()
      out.toByteArray
    }
  }

  implicit class PBReadsOps(bytes: Array[Byte]) {
    def parseTo[A: PBReads]: A = {
      val from = com.google.protobuf.CodedInputStream.newInstance(bytes)
      PBReads[A].read(from)
    }
  }

}
