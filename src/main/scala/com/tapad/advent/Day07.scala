package com.tapad.advent

import cats.implicits._
import cats.effect.IO
import fs2.Stream

object Day07 extends AventDay {
  override val filename: String = "day07.txt"
  override val testInput: Stream[IO, String] = Stream("16","1","2","0","4","2","7","1","2","14")

  override val partition = coma

  override val testOutput1: Long = 37
  override def task1(input: Stream[IO, String]): IO[Long] = {
    input.map(_.toInt).compile.toList.map { initial =>
      val fuel = for {
        position <- initial.min to initial.max
      } yield {
        initial
          .map(i => math.abs(i - position))
          .combineAll
      }
      fuel.min
    }
  }

  override val testOutput2: Long = 168
  override def task2(input: Stream[IO, String]): IO[Long] = {
    input.map(_.toInt).compile.toList.map { initial =>
      val candidates = for {
        position <- initial.min to initial.max
      } yield {
        initial.foldLeft(0) { (acc, i) =>
          val distance = math.abs(i - position)
          val fuel = (1 to distance).sum
          acc + fuel
        }
      }
      candidates.min
    }
  }
}
