package pbmeta

import scala.meta._
import scala.collection.immutable.Seq

class Class2Map extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls @ Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        val namesToValues: Seq[Term.Tuple] = paramss.flatten.map { param =>
          q"(${param.name.syntax}, ${Term.Name(param.name.value)})"
        }
        val toMapImpl: Term =
          q"_root_.scala.collection.Map[String, Any](..$namesToValues)"
        val toMap =
          q"def toMap: _root_.scala.collection.Map[String, Any] = $toMapImpl"
        val templateStats: Seq[Stat] = toMap +: template.stats.getOrElse(Nil)
        cls.copy(templ = template.copy(stats = Some(templateStats)))
      case _ =>
        println(defn.structure)
        abort("@Class2Map must annotate a class.")
    }
  }
}


class WithApply extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    def createApply(name: Type.Name, paramss: Seq[Seq[Term.Param]]): Defn.Def = {
      val args = paramss.map(_.map(param => Term.Name(param.name.value)))
      q"""def apply(...$paramss): $name =
            new ${Ctor.Ref.Name(name.value)}(...$args)"""
    }
    defn match {
      // companion object exists
      case Term.Block(
      Seq(cls @ Defn.Class(_, name, _, ctor, _),
      companion: Defn.Object)) =>
        val applyMethod = createApply(name, ctor.paramss)
        val templateStats: Seq[Stat] =
          applyMethod +: companion.templ.stats.getOrElse(Nil)
        val newCompanion = companion.copy(
          templ = companion.templ.copy(stats = Some(templateStats)))
        Term.Block(Seq(cls, newCompanion))
      // companion object does not exists
      case cls @ Defn.Class(_, name, _, ctor, _) =>
        val applyMethod = createApply(name, ctor.paramss)
        val companion   = q"object ${Term.Name(name.value)} { $applyMethod }"
        Term.Block(Seq(cls, companion))
      case _ =>
        println(defn.structure)
        abort("@WithApply must annotate a class.")
    }
  }
}

class PBWritable extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    def createWriteField(p: (Term.Param, Int)): Seq[Term.Apply] = p match {
        case (param"..$mod $pname: $ptype", index) if ptype.exists(_.syntax == "Boolean") =>
          Seq(q"pbOut.writeBool(${Lit.Int(index+1)}, ${Term.Name(pname.syntax)})")
        case (param"..$mod $pname: $ptype", index) if ptype.exists(_.syntax == "Int")     =>
          Seq(q"pbOut.writeInt32(${Lit.Int(index+1)}, ${Term.Name(pname.syntax)})")
        case (param"..$mod $pname: $ptype", index) if ptype.exists(_.syntax == "String")  =>
          Seq(q"pbOut.writeString(${Lit.Int(index+1)}, ${Term.Name(pname.syntax)})")
        case (param"..$mod $pname: $ptype", index) if ptype.exists(_.syntax.endsWith(".Value")) =>
          Seq(q"pbOut.writeInt32(${Lit.Int(index+1)}, ${Term.Name(pname.syntax)}.id)")
        case _ => Seq.empty
      }
    def createToPBMethod(paramss: Seq[Seq[Term.Param]]): Defn.Def = {
      val fields: Seq[Term.Apply] = paramss.head.zipWithIndex.flatMap(createWriteField)
      q"""
         def toPB: Array[Byte] = {
           val out = new java.io.ByteArrayOutputStream
           val pbOut = com.google.protobuf.CodedOutputStream.newInstance(out)
           ..$fields
           pbOut.flush()
           out.toByteArray
         }
       """
    }
    def declareReadField(p: Term.Param): Defn.Var = p match {
      case param"..$mod $pname: $ptype" => q"var ${Pat.Var.Term(Term.Name(pname.value))}: Option[${Type.Name(ptype.get.syntax)}] = None"
    }
    def createReadField(p: (Term.Param, Int)): Seq[Case] = p match {
      case (param"..$mod $pname: $ptype", index) if ptype.exists(_.syntax == "Boolean") =>
        Seq(p"case tag if (tag >> 3) == ${Lit.Int(index + 1)} => ${Term.Name(pname.value)} = Some(pbIn.readBool)")
      case (param"..$mod $pname: $ptype", index) if ptype.exists(_.syntax == "Int") =>
        Seq(p"case tag if (tag >> 3) == ${Lit.Int(index + 1)} => ${Term.Name(pname.value)} = Some(pbIn.readInt32)")
      case (param"..$mod $pname: $ptype", index) if ptype.exists(_.syntax == "String") =>
        Seq(p"case tag if (tag >> 3) == ${Lit.Int(index + 1)} => ${Term.Name(pname.value)} = Some(pbIn.readString)")
      case (param"..$mod $pname: $ptype", index) if ptype.exists(_.syntax.endsWith(".Value")) =>
        Seq(p"case tag if (tag >> 3) == ${Lit.Int(index + 1)} => ${Term.Name(pname.value)} = Some(${Ctor.Ref.Name(ptype.get.syntax.stripSuffix(".Value"))}(pbIn.readInt32))")
//      case (param"..$mod $pname: $ptype", index) if ptype.
    }
    def createReadArg(p: Term.Param): Term.Apply = {
      val message = s""""Missing required field ${p.name.value}""""
      q"${Term.Name(p.name.value)}.getOrElse(throw new IllegalArgumentException($message))"

    }
    def createFromPBMethod(name: Type.Name, ctor: Ctor.Primary): Defn.Def = {
      val declaredFields: Seq[Defn.Var] = ctor.paramss.head.map(declareReadField)
      val cases: Seq[Case] = ctor.paramss.head.zipWithIndex.flatMap(createReadField)
      val args: Seq[Term.Apply] = ctor.paramss.head.map(createReadArg)
      q"""
          def fromPB(bytes: Array[Byte]): $name = {
            var done = false
            val pbIn = com.google.protobuf.CodedInputStream.newInstance(bytes)
            ..$declaredFields
            while (!done) {
              pbIn.readTag match {
                case 0 => done = true
                ..case $cases
                case tag => pbIn.skipField(tag)
              }
            }
            new ${Ctor.Ref.Name(name.value)}(..$args)
          }
        """
    }
    def enumToPB: Defn.Def =
      q"""
        def toPB(value: Value): Array[Byte] = {
          val out = new java.io.ByteArrayOutputStream
          val pbOut = com.google.protobuf.CodedOutputStream.newInstance(out)
          pbOut.writeInt32(1, value.id)
          pbOut.flush()
          out.toByteArray
        }
        """
    def pbToEnum(name: Term.Name): Defn.Def =
      q"""
        def fromPB(bytes: Array[Byte]): Value = {
          var done = false
          val pbIn = com.google.protobuf.CodedInputStream.newInstance(bytes)
          var id = -1
          while (!done) {
            pbIn.readTag match {
              case 0 => done = true
              case tag if (tag >> 3) == 1 => id = pbIn.readInt32
              case tag => pbIn.skipField(tag)
            }
          }
          $name(id)
         }
       """
    defn match {
      case Term.Block(Seq(cls@Defn.Class(_, name, _, ctor, template), companion: Defn.Object)) =>
        // companion object exists
        val toPB = createToPBMethod(ctor.paramss)
        val templateStats: Seq[Stat] = toPB +: template.stats.getOrElse(Nil)
        val newClass = cls.copy(templ = template.copy(stats = Some(templateStats)))
        val fromPB = createFromPBMethod(name, ctor)
        val newCompanion = companion.copy(templ = companion.templ.copy(stats = Some(companion.templ.stats.getOrElse(Seq.empty) :+ fromPB)))
        Term.Block(Seq(newClass, newCompanion))
      case cls@Defn.Class(_, name, _, ctor, template) =>
        // companion object doesn't exists
        val toPB = createToPBMethod(ctor.paramss)
        val templateStats: Seq[Stat] = toPB +: template.stats.getOrElse(Nil)
        val newClass = cls.copy(templ = template.copy(stats = Some(templateStats)))
        val fromPB = createFromPBMethod(name, ctor)
        val newCompanion = q"object ${Term.Name(name.value)} { $fromPB }"
        Term.Block(Seq(newClass, newCompanion))
      case obj@Defn.Object(_, name, templ) if templ.parents.map(_.syntax).contains("Enumeration()") =>
        obj.copy(templ = templ.copy(stats = Some(templ.stats.getOrElse(Nil) :+ enumToPB :+ pbToEnum(name))))
    }
  }
}