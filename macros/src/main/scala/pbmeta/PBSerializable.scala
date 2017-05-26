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
                  var i = 1
                  ..${params.zipWithIndex.map(writeField)}
               }
            }
       """
    }

    def writeField(p: (Term.Param, Int)): Term.Apply = {
      val (param"$pname: $ptype", i) = p
      val typeName = Type.Name(ptype.get.syntax)
      val name  = Term.Name(pname.value)
      val index = Lit.Int(i+1)
      q"pbmeta.PBWrites[$typeName].write(a.$name, to, Some($index))"
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

    def declareField(p: Term.Param): Defn.Var = {
      val param"$pname: $ptype" = p
      val typeName = Type.Name(ptype.get.syntax)
      q"var ${Pat.Var.Term(Term.Name(pname.value))}: List[$typeName] = Nil"
    }

    def readField(p: (Term.Param, Int)): Case = {
      val (param"$pname: $ptype", i) = p
      val typeName = Type.Name(ptype.get.syntax)
      val name = Term.Name(pname.value)
      val index = Lit.Int(i+1)
      p"case tag if (tag >> 3) == $index => $name ::= pbmeta.PBReads[$typeName].read(from)"
    }

    def extractField(p: Term.Param): Term.Arg = {
      val param"$pname: $ptype = $dfault" = p
      val prefix = ptype.get.syntax.takeWhile(_ != '[')
      val repeatedTypes = Set("List", "Seq", "Set")
      val name = Term.Name(pname.value)
      val error = s""""Missing required field ${pname.value}""""
      val alt = dfault.getOrElse(q"throw new IllegalArgumentException($error)")
      prefix match {
        case t if repeatedTypes.contains(t) => arg"$name = $name.reverse"
        case "Option" => arg"$name = $name.headOption"
        case _ => arg"$name = $name.headOption.getOrElse($alt)"
      }
    }

    defn match {
      case Term.Block(Seq(cls@Defn.Class(_, name, _, ctor, template), companion: Defn.Object)) =>
        // companion object exists
        val reads = prodReads(name, ctor)
        var writes = prodWrites(name, ctor.paramss.head)
        val stats = companion.templ.stats.getOrElse(Seq.empty)
        val newCompanion = companion.copy(templ = companion.templ.copy(stats = Some(stats :+ reads :+ writes)))
        Term.Block(Seq(cls, companion))
      case cls@Defn.Class(_, name, _, ctor, template) =>
        // companion object doesn't exist
        val newCompanion =
          q"""
            object ${Term.Name(name.value)} {
              ${prodReads(name, ctor)}
              ${prodWrites(name, ctor.paramss.head)}
            }
           """
        Term.Block(Seq(cls, newCompanion))
      case obj@Defn.Object(_, name, templ) if templ.parents.map(_.syntax).contains("Enumeration()") =>
        // Scala enumeration
        obj.copy(templ = templ.copy(stats = Some(templ.stats.getOrElse(Nil) :+ enumReads(name))))
    }
  }

}
