package com.tapad.advent.day03

import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import fs2.io.file.{Files, Path}

object PartOne extends IOApp {

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

//     fileInput
    testInput
      .map(toIndexCount)
      .reduce { (v1, v2) =>
        (v1 zip v2) map {
          case (first, second) =>
            first |+| second
        }
      }
      .compile
      .lastOrError
      .flatMap { vector =>
        val gammaBin = vector.map(most)
        val epsilonBin = vector.map(least)

        val gamma = toLong(gammaBin)
        val epsilon = toLong(epsilonBin)

        info("gamma binary: " + gammaBin.mkString) *>
          info("epsilon binary: " + epsilonBin.mkString) *>
          info("gamma: " + gamma) *>
          info("epsilon: " + epsilon) *>
          info("product: " + (gamma * epsilon))
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
}