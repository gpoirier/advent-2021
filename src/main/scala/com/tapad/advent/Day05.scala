package com.tapad.advent

import cats.effect.IO
import cats.implicits._
import fs2.Stream

object Day05 extends AventDay {
  override val filename: String = "day05.txt"
  override val testInput: Stream[IO, String] = Stream(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )

  override val testOutput1: Long = 5
  override def task1(input: Stream[IO, String]): IO[Long] =
    input
      .evalMap(parse)
      .map(_.linesToMatrix)
      .compile
      .foldMonoid
      .map(_.count(_._2 > 1))

  override val testOutput2: Long = 12
  override def task2(input: Stream[IO, String]): IO[Long] =
    input
      .evalMap(parse)
      .map(_.toMatrix)
      .compile
      .foldMonoid
      .map(_.count(_._2 > 1))

  import scala.util.parsing.combinator._
  object Parser extends JavaTokenParsers {
    val line: Parser[Line] = wholeNumber ~ "," ~ wholeNumber ~ "->" ~ wholeNumber ~ "," ~ wholeNumber ^^ {
      case x1 ~ _ ~ y1 ~ _ ~ x2 ~ _ ~ y2 => Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }

    def apply(text: String): ParseResult[Line] = parseAll(line, text)
  }

  case class Position(x: Int, y: Int)
  type Count = Int

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
    def linesToMatrix: Map[Position, Count] = {
      if (x1 == x2) {
        val start = math.min(y1, y2)
        val end = math.max(y1, y2)
        (start to end).map(y => Position(x1, y) -> 1).toMap
      } else if (y1 == y2) {
        val start = math.min(x1, x2)
        val end = math.max(x1, x2)
        (start to end).map(x => Position(x, y1) -> 1).toMap
      } else {
        Map.empty
      }
    }
    def toMatrix: Map[Position, Count] = {
      val stepX =
        if (x1 == x2) 0
        else if (x1 > x2) -1
        else 1
      val stepY =
        if (y1 == y2) 0
        else if (y1 > y2) -1
        else 1

      val last = Position(x2, y2)
      def go(current: Position): Map[Position, Count] =
        if (current == last) Map(current -> 1)
        else go(Position(current.x + stepX, current.y + stepY)) ++ Map(current -> 1)

      go(Position(x1, y1))
    }
  }

  def parse(line: String): IO[Line] = IO(Parser(line).getOrElse(sys.error(s"Failed to parse line: $line")))
}
