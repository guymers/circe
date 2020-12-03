package io.circe

import cats.syntax.eq._
import io.circe.tests.CirceSuite
import org.scalacheck.Prop
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class PrinterWriterReuseSuite extends CirceSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMaxSize(100)

  property("Printer#reuseWriters should not change the behavior of print") {
    Prop.forAll { (values: List[Json]) =>
      val default = Printer.spaces4
      val reusing = default.copy(reuseWriters = true)
      val expected = values.map(default.print)

      val result = Future.traverse(values)(value => Future(reusing.print(value)))
      assert(Await.result(result, 1.second) === expected)
    }
  }
}
