package com.tapad.advent.day03

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.{Pure, Stream}
import fs2.io.file.{Files, Path}

import scala.annotation.tailrec

object PartTwo extends IOApp {

  val testInput: Stream[IO, String] = Stream(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010",
  )

  override def run(args: List[String]): IO[ExitCode] = {
    val path = Path("src/main/resources/com/tapad/advent/day03/day03.txt")

    val fileInput = Files[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)


    @tailrec
    def go(input: List[String], index: Int = 0)(oxygen: Boolean): String = {
      val sum = input
        .map(toIndexCount)
        .map(_.apply(index))
        .combineAll

      val zeros = sum('0')
      val ones = sum('1')
      val current =
        if (oxygen) {
          if (zeros > ones) '0' else '1'
        } else {
          if (zeros > ones) '1' else '0'
        }

      val filtered = input.filter(_.apply(index) === current)
      if (filtered.size == 1) filtered.head
      else go(filtered, index + 1)(oxygen)
    }

    def findOxygen(input: List[String]): String = go(input, 0)(oxygen = true)

    def findCo2(input: List[String]): String = go(input, 0)(oxygen = false)

    fileInput.compile.toList flatMap { list =>
      val oxygen = findOxygen(list)
      val co2 = findCo2(list)
      val oxygenLong = toLong(oxygen.toVector)
      val co2Long = toLong(co2.toVector)
      IO(println("oxygen: " + oxygen)) *>
      IO(println("CO2: " + co2)) *>
      IO(println("Product: " + (oxygenLong * co2Long)))
        .as(ExitCode.Success)
    }
  }

  def toLong(v: Vector[Char]): Long = {
    v.reverse.zipWithIndex.map {
      case ('0', _) => 0
      case ('1', index) => math.pow(2, index).toLong
    }.sum
  }

  private def info(text: String): IO[Unit] = IO(println("[info] " + text))

  def least(m: Map[Char, Int]): Char = m.minBy(_._2)._1
  def most(m: Map[Char, Int]): Char = m.maxBy(_._2)._1

  def toIndexCount(line: String): Vector[Map[Char, Int]] = {
    line.toVector.map {
      case '0' => Map('0' -> 1, '1' -> 0)
      case '1' => Map('0' -> 0, '1' -> 1)
    }
  }

  case class RichLine(line: String) {
    val count: Vector[Map[Char, Int]] = toIndexCount(line)
  }
}