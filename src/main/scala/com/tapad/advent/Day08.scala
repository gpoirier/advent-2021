package com.tapad.advent

import cats.implicits._
import cats.effect.IO
import fs2.Stream

object Day08 extends AventDay {
  override val filename: String = "day08-full.txt"
  override val testInput: Stream[IO, String] = readFile("day08-test.txt")

  val partialSizeToDigit = Map(
    2 -> 1,
    3 -> 7,
    4 -> 4,
    7 -> 8
  )

  val sizeToDigit = Map(
    2 -> List(1),
    3 -> List(7),
    4 -> List(4),
    5 -> List(2, 3, 5),
    6 -> List(0, 6, 9),
    7 -> List(8)
  )

  override val testOutput1: Long = 26
  override def task1(input: Stream[IO, String]): IO[Long] = {
    val patterns = input.flatMap { line =>
      val parts = line.split("\\|")
      val output = parts(1)
      Stream(output.split(" "): _*)
    }
    patterns
      .filter(p => partialSizeToDigit.contains(p.length))
      .compile
      .count
  }

  override val testOutput2: Long = 61229
  override def task2(input: Stream[IO, String]): IO[Long] = {
    val outputs = input.map { line =>
      val parts = line.split("\\|")
      val training = parts(0).split(" ").toList
      val one = training.find(_.length == 2).get
      val four = training.find(_.length == 4).get
      val seven = training.find(_.length == 3).get

      val bd = four.toSet diff one.toSet

      def toDigit(pattern: String): Int = {
        val size = pattern.length
        partialSizeToDigit.getOrElse(size, {
          size match {
            case 5 =>
              // 2, 3, and 5 have 5 segments
              if (bd.forall(c => pattern.contains(c))) 5
              else if (seven.forall(c => pattern.contains(c))) 3
              else 2
            case 6 =>
              // 0, 6 and 9 have 6 segments
              if (!seven.forall(c => pattern.contains(c))) 6
              else if (four.forall(c => pattern.contains(c))) 9
              else 0
          }
        })
      }
      val output = parts(1).split(" ").toList.map(_.trim).filter(_.nonEmpty)
      output.map(toDigit).mkString.toLong
    }
    outputs.compile.foldMonoid
  }
}
