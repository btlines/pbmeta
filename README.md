[![Build status](https://api.travis-ci.org/btlines/pbmeta.svg?branch=master)](https://travis-ci.org/btlines/pbmeta)
[![codecov](https://codecov.io/gh/btlines/pbmeta/branch/master/graph/badge.svg)](https://codecov.io/gh/btlines/pbmeta)
[![Dependencies](https://app.updateimpact.com/badge/852442212779298816/pbmeta.svg?config=compile)](https://app.updateimpact.com/latest/852442212779298816/pbmeta)
[![License](https://img.shields.io/:license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Download](https://api.bintray.com/packages/beyondthelines/maven/pbmeta/images/download.svg) ](https://bintray.com/beyondthelines/maven/pbmeta/_latestVersion)

# PBMeta

Read/Write Scala objects directly to Protobuf with no .proto file definitions

## Context

Protobuf is a fast and efficient way to serialize data. While .proto files are great to share schema definitions between components, it is sometimes much simpler and straightforward to directly encode Scala object without using a .proto schema definition file.

PBMeta aims just that: Make it easier to serialize/deserialize into Protobuf.

## Setup

In order to use PBMeta you need to add the following lines to your `build.sbt`:

```scala
resolvers += Resolver.bintrayRepo("beyondthelines", "maven")

libraryDependencies += "beyondthelines" %% "pbmeta" % "0.0.1"
```

## Dependencies

PBDirect depends on:
 - [protobuf-java](https://developers.google.com/protocol-buffers/docs/javatutorial) the Protobuf java library (maintained by Google) 
 - [scalameta](https://github.com/scalameta/scalameta) for the `PBFormat` generation macros
 
## Usage

In order to use PBMeta you need to import the following:

```scala
import pbmeta._
```

This import makes the annotation `@PBSerializable` available. You can then annotate your data model case classes with `@PBSerializable`.
This annotation triggers a macro that creates implicit `PBReads` and `PBWrites` instances in the companion object.

Generated `PBFormats` are similar to [play-json](https://github.com/playframework/play-json) formats:
* `PBReads` is a type class used to read protobuf binary data and turn it into a case class instance
* `PBWrites` is a type class used to write a case class instance into protobuf binary format
* `PBFormat` is a combination of `PBReads` and `PBWrites`

## Example

### Schema definition

PBDirect serialises case classes into protobuf and there is no need for a .proto shema definition file.

```scala
@PBSerializable
case class MyMessage(
  id: Option[Int], 
  text: Option[String], 
  numbers: List[Int]
)
```

is equivalent to the following protobuf definition:

```protobuf
message MyMessage {
   optional int32  id      = 1;
   optional string text    = 2;
   repeated int32  numbers = 3;
}
```

The field numbers correspond to the order of the fields inside the case class.

### Serialization

You only need to call the `toPB` method on your case class. This method is implicitly added with `import pbmeta._`.

```scala
val message = MyMessage(
  id = Some(123),
  text = Some("Hello"),
  numbers = List(1, 2, 3, 4)
)
val bytes = message.toPB
```

### Deserialization

Deserializing bytes into a case class is also straight forward. You only need to call the `pbTo[A]` method on the byte array containing the protobuf encoded data.
This method is added implicitly on all `Array[Byte]` by importing `pbmeta._`.

```scala
val bytes: Array[Byte] = Array[Byte](8, 123, 18, 5, 72, 101, 108, 108, 111, 24, 1, 32, 2, 40, 3, 48, 4)
val message = bytes.pbTo[MyMessage]
```