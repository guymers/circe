package io.circe.tests

import cats.syntax.EitherOps
import io.circe.testing.{ ArbitraryInstances, EqInstances }
import munit.DisciplineSuite
import munit.ScalaCheckSuite
import scala.language.implicitConversions

/**
 * An opinionated stack of traits to improve consistency and reduce boilerplate in circe tests.
 */
abstract class CirceSuite
    extends ScalaCheckSuite
    with DisciplineSuite
    with ArbitraryInstances
    with EqInstances
    with MissingInstances {

  implicit def prioritizedCatsSyntaxEither[A, B](eab: Either[A, B]): EitherOps[A, B] = new EitherOps(eab)
}
