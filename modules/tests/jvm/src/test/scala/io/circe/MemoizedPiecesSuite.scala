package io.circe

import io.circe.tests.CirceSuite
import org.scalacheck.{Arbitrary, Gen, Prop}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class MemoizedPiecesSuite extends CirceSuite {

  case class Depths(depths: Seq[Int])

  object Depths {
    implicit val arbitraryDepths: Arbitrary[Depths] = Arbitrary(
      Gen.containerOfN[Seq, Int](512, Gen.choose(0, 256)).map(Depths(_))
    )
  }

  def makePieces: Printer.MemoizedPieces = new Printer.MemoizedPieces(" ") {
    def compute(i: Int): Printer.Pieces = new Printer.Pieces(
      "%s%s%s".format(" " * i, "a", " " * (i + 1)),
      "%s%s%s".format(" " * i, "b", " " * (i + 1)),
      "%s%s%s".format(" " * i, "c", " " * (i + 1)),
      "%s%s%s".format(" " * i, "d", " " * (i + 1)),
      "%s%s%s".format(" " * i, "e", " " * (i + 1)),
      "%s%s%s".format(" " * i, "f", " " * (i + 1)),
      "%s%s%s".format(" " * i, "g", " " * (i + 1)),
      "%s%s%s".format(" " * i, "h", " " * (i + 1))
    )
  }

  val pieces: Printer.MemoizedPieces = {
    val tmp = makePieces
    makePieces(256)
    tmp
  }

  property("Printer.MemoizedPieces should should be correct for arbitrarily ordered depths under concurrent usage") {
    Prop.forAll { (depths: Depths) =>
      val newPieces = makePieces

      val result = Future.traverse(depths.depths)(depth => Future(newPieces(depth)))
      assert(Await.result(result, 1.second).sameElements(depths.depths.map(pieces(_))))
    }
  }
}
