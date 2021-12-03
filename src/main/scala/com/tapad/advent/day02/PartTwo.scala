package com.tapad.advent.day02

import cats.effect._
import fs2.Stream
import fs2.io.file.{Path, Files}

object PartTwo extends IOApp {

  val testInput: Stream[IO, String] = Stream(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )

  override def run(args: List[String]): IO[ExitCode] = {
    val path = Path("src/main/resources/com/tapad/advent/day02/day02.txt")

    val fileInput = Files[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)

    fileInput
      .map(parse)
      .compile
      .fold(Position.empty) { (acc, direction) =>
        direction match {
          case Forward(value) => acc.copy(horizontal = acc.horizontal + value, depth = acc.depth + acc.aim * value)
          case Down(value) => acc.copy(aim = acc.aim + value)
          case Up(value) => acc.copy(aim = acc.aim - value)
        }
      }
      .flatMap { sum =>
        IO(println("Final position: " + sum)) *>
          IO(println("Product: " + (sum.horizontal * sum.depth))).as(ExitCode.Success)
      }
  }

  def parse(line: String): Direction = {
    val Array(direction, value) = line.split(" ")
    direction match {
      case "forward" => Forward(value.toLong)
      case "up" => Up(value.toLong)
      case "down" => Down(value.toLong)
    }
  }

  sealed trait Direction {
    val value: Long
  }

  case class Forward(value: Long) extends Direction
  case class Down(value: Long) extends Direction
  case class Up(value: Long) extends Direction

  case class Position(horizontal: Long, depth: Long, aim: Long)
  object Position {
    val empty = Position(0, 0, 0)
  }
}
