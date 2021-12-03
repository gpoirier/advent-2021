package com.tapad.advent.day02

import cats._
import cats.effect._
import fs2.Stream
import fs2.io.file.{Path, Files}

object PartOne extends IOApp {

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
      .map(_.toPosition)
      .compile
      .foldMonoid
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

    def toPosition: Position
  }

  case class Forward(value: Long) extends Direction {
    override def toPosition: Position = Position(value, 0)
  }

  case class Down(value: Long) extends Direction {
    override def toPosition: Position = Position(0, value)
  }

  case class Up(value: Long) extends Direction {
    override def toPosition: Position = Position(0, -value)
  }

  case class Position(horizontal: Long, depth: Long)

  object Position {
    val empty = Position(0, 0)
    implicit val PositionMonoid: Monoid[Position] =
      Monoid.instance(
        empty,
        (first, second) => Position(first.horizontal + second.horizontal, first.depth + second.depth)
      )
  }
}
