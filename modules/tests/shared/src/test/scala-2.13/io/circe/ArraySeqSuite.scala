package io.circe

import cats.Eq
import io.circe.syntax.EncoderOps
import io.circe.testing.CodecTests
import io.circe.tests.CirceSuite
import org.scalacheck.Prop

import scala.collection.immutable.ArraySeq

class ArraySeqSuite extends CirceSuite {

  def decodeArraySeqWithoutClassTag[A: Decoder](json: Json): Decoder.Result[ArraySeq[A]] =
    json.as[ArraySeq[A]]

  property("decoding an arraySeq should succeed when the type is fully specified") {
    Prop.forAll { int: Int =>
      assert(Json.arr(int.asJson).as[ArraySeq[Int]] === Right(ArraySeq(int)))
    }
  }

  property("decoding an arraySeq should succeed for polymorphic decoders") {
    Prop.forAll { string: String =>
      assert(decodeArraySeqWithoutClassTag[String](Json.arr(string.asJson)) === Right(ArraySeq(string)))
    }
  }

  property("decoding an arraySeq should specialise the array type where a class tag is available") {
    Prop.forAll { intArray: Array[Int] =>
      val jsonArray = Json.arr(intArray.map(_.asJson): _*)

      assert(jsonArray.as[ArraySeq[Int]].map(_.getClass) == Right(classOf[ArraySeq.ofInt]))
    }
  }

  property("decoding an arraySeq should not specialise the array type where no class tag is available") {
    Prop.forAll { intArray: Array[Int] =>
      val jsonArray = Json.arr(intArray.map(_.asJson): _*)

      assert(decodeArraySeqWithoutClassTag[Int](jsonArray).map(_.getClass) == Right(classOf[ArraySeq.ofRef[_]]))
    }
  }

  checkAll("Codec[ArraySeq[Int]]", CodecTests[ArraySeq[Int]].codec)
  checkAll("Codec[ArraySeq[String]]", CodecTests[ArraySeq[String]].codec)

}
