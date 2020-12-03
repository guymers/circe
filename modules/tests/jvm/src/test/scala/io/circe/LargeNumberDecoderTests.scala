package io.circe

import io.circe.parser.parse
import io.circe.tests.CirceSuite
import org.scalacheck.Prop

/**
 * Tests that fail because of bugs (or at least limitations) on Scala.js.
 */
trait LargeNumberDecoderTests { this: CirceSuite =>
  property("Decoder[Long] should succeed on whole decimal values (#83)") {
    Prop.forAll { (v: Long, n: Byte) =>
      val zeros = "0" * (math.abs(n.toInt) + 1)
      val Right(json) = parse(s"$v.$zeros")

      assert(Decoder[Long].apply(json.hcursor) === Right(v))
    }
  }

  property("Decoder[BigInt] should succeed on whole decimal values (#83)") {
    Prop.forAll { (v: BigInt, n: Byte) =>
      val zeros = "0" * (math.abs(n.toInt) + 1)
      val Right(json) = parse(s"$v.$zeros")

      assert(Decoder[BigInt].apply(json.hcursor) === Right(v))
    }
  }
}
