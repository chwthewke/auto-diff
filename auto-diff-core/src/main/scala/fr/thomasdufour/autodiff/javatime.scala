package fr.thomasdufour.autodiff

import java.time.Duration
import java.time.Instant
import java.time.OffsetDateTime

object javatime extends javatime

trait javatime {
  implicit val instantDiffShow: DiffShow.Aux[Instant, ValueDifference] =
    DiffShow.default

  implicit val offsetDateTimeDiffShow: DiffShow.Aux[OffsetDateTime, ValueDifference] =
    DiffShow.default

  implicit val durationDiffShow: DiffShow.Aux[Duration, ValueDifference] =
    DiffShow.default
}
