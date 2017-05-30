package pbmeta

import scala.collection.immutable.Seq
import scala.meta._

class PBSerializable extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    def enumReads(name: Term.Name): Defn.Val =
      q"implicit val pbReads: pbmeta.PBReads[Value] = pbmeta.PBReads.enum($name)"

    def prodWrites(name: Type.Name, params: Seq[Term.Param]): Defn.Val = {
      q"""
          implicit val pbWrites: pbmeta.PBWrites[$name] =
            new pbmeta.PBWrites[$name] {
               override def write(a: $name, to: com.google.protobuf.CodedOutputStream, at: Option[Int]): Unit = {
                  at.foreach { i =>
                    to.writeTag(i, com.google.protobuf.WireFormat.WIRETYPE_LENGTH_DELIMITED)
                    to.writeUInt32NoTag(sizeOf(a))
                  }
                  ..${params.zipWithIndex.map(writeField)}
               }
               override def sizeOf(a: $name, at: Option[Int]): Int = {
                  val sizes: Seq[Int] = Seq(..${params.zipWithIndex.map(sizeField)})
                  sizes.reduceOption(_+_).getOrElse(0) +
                  at.map(com.google.protobuf.CodedOutputStream.computeTagSize).getOrElse(0)
               }
            }
       """
    }

    def sizeField(p: (Term.Param, Int)): Term.Apply = {
      val (param"$pname: $ptype", i) = p
      val typeName = innerType(ptype.get)
      val prefix = ptype.get.syntax.takeWhile(_ != '[')
      val name  = Term.Name(pname.value)
      val index = Lit.Int(i+1)
      prefix match {
        case "List" | "Map" | "Option" | "Seq" | "Set" =>
          q"a.$name.map(v => pbmeta.PBWrites[$typeName].sizeOf(v, Some($index))).reduceOption(_+_).getOrElse(0)"
        case _ =>
          q"pbmeta.PBWrites[$typeName].sizeOf(a.$name, Some($index))"
      }
    }

    def writeField(p: (Term.Param, Int)): Term.Apply = {
      val (param"$pname: $ptype", i) = p
      val typeName = innerType(ptype.get)
      val prefix = ptype.get.syntax.takeWhile(_ != '[')
      val name  = Term.Name(pname.value)
      val index = Lit.Int(i+1)
      prefix match {
        case "List" | "Map" | "Option" | "Seq" | "Set" =>
          q"a.$name.foreach(v => pbmeta.PBWrites[$typeName].write(v, to, Some($index)))"
        case _ =>
          q"pbmeta.PBWrites[$typeName].write(a.$name, to, Some($index))"
      }
    }

    def prodReads(name: Type.Name, ctor: Ctor.Primary): Defn.Val = {
      val fields: Seq[Defn.Var] = ctor.paramss.head.map(declareField)
      val cases: Seq[Case] = ctor.paramss.head.zipWithIndex.map(readField)
      val args = ctor.paramss.head.map(extractField)
      val constructor = Ctor.Ref.Name(name.value)
      q"""
         implicit val pbReads: pbmeta.PBReads[$name] =
           new pbmeta.PBReads[$name] {
             override def read(from: com.google.protobuf.CodedInputStream): $name = {
               var done = false
               ..$fields
               while (!done) {
                 from.readTag match {
                   case 0 => done = true
                   ..case $cases
                   case tag => from.skipField(tag)
                 }
               }
               new $constructor(..$args)
             }
           }
       """
    }

    def innerType(ptype: Type.Arg): Type = {
      val option = """Option\[(.+)]""".r
      val list   = """List\[(.+)\]""".r
      val seq    = """Seq\[(.+)\]""".r
      val set    = """Set\[(.+)\]""".r
      val map    = """Map\[(.+),\s*(.+)\]""".r
      ptype.syntax match {
        case option(tpe) => Type.Name(tpe)
        case list(tpe)   => Type.Name(tpe)
        case seq(tpe)    => Type.Name(tpe)
        case set(tpe)    => Type.Name(tpe)
        case map(k,v)    => t"(..${Seq(Type.Name(k), Type.Name(v))})"
        case tpe         => Type.Name(tpe)
      }
    }

    def declareField(p: Term.Param): Defn.Var = {
      val param"$pname: $ptype" = p
      val typeName = innerType(ptype.get)
      q"var ${Pat.Var.Term(Term.Name(pname.value))}: List[$typeName] = Nil"
    }

    def readField(p: (Term.Param, Int)): Case = {
      val (param"$pname: $ptype", i) = p
      val typeName = innerType(ptype.get)
      val name = Term.Name(pname.value)
      val index = Lit.Int(i+1)
      val knownTypes = Set("Boolean", "Int", "Long", "Float", "Double", "String", "Array[Byte]")
      val prefix = ptype.get.syntax.takeWhile(_ != ']')
      if (prefix == "Map" || (!knownTypes.contains(typeName.syntax) && !typeName.syntax.endsWith(".Value"))) {
        p"case tag if (tag >> 3) == $index => from.readUInt32(); $name ::= pbmeta.PBReads[$typeName].read(from)"
      } else {
        p"case tag if (tag >> 3) == $index => $name ::= pbmeta.PBReads[$typeName].read(from)"
      }
    }

    def extractField(p: Term.Param): Term.Arg = {
      val param"$pname: $ptype = $dfault" = p
      val prefix = ptype.get.syntax.takeWhile(_ != '[')
      val name = Term.Name(pname.value)
      val error = s""""Missing required field ${pname.value}""""
      val alt = dfault.getOrElse(q"throw new IllegalArgumentException($error)")
      prefix match {
        case "List"   => arg"$name = $name.reverse"
        case "Seq"    => arg"$name = $name.reverse.toSeq"
        case "Set"    => arg"$name = $name.toSet"
        case "Map"    => arg"$name = $name.toMap"
        case "Option" => arg"$name = $name.headOption"
        case _        => arg"$name = $name.headOption.getOrElse($alt)"
      }
    }

    defn match {
      case Term.Block(Seq(cls@Defn.Class(_, name, _, ctor, _), companion: Defn.Object)) =>
        // companion object exists
        val reads = prodReads(name, ctor)
        var writes = prodWrites(name, ctor.paramss.head)
        val stats = companion.templ.stats.getOrElse(Seq.empty)
        val newCompanion = companion.copy(templ = companion.templ.copy(stats = Some(stats :+ reads :+ writes)))
        Term.Block(Seq(cls, newCompanion))
      case cls@Defn.Class(_, name, _, ctor, _) =>
        // companion object doesn't exist
        val newCompanion =
          q"""
            object ${Term.Name(name.value)} {
              ${prodReads(name, ctor)}
              ${prodWrites(name, ctor.paramss.head)}
            }
           """
        Term.Block(Seq(cls, newCompanion))
      case obj@Defn.Object(_, name, template) if template.parents.map(_.syntax).contains("Enumeration()") =>
        // Scala enumeration
        obj.copy(templ = template.copy(stats = Some(template.stats.getOrElse(Nil) :+ enumReads(name))))
    }
  }

}
