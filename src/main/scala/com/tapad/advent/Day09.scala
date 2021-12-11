package com.tapad.advent

import cats.implicits._
import cats.effect.IO
import fs2._

object Day09 extends AventDay {
  override val filename: String = "day09.txt"
  override val testInput: Stream[IO, String] = Stream(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  )

  override val testOutput1: Long = 15
  override def task1(input: Stream[IO, String]): IO[Long] = {
    input.compile.toVector map { vector =>
      val digits = for {
        y <- vector.indices
        line = vector(y)
        x <- line.indices
        digit = line(x).toString.toInt
        floor = {
          val up = vector.get(y - 1).map(_.charAt(x)).map(_.toString.toInt)
          val down = vector.get(y + 1).map(_.charAt(x)).map(_.toString.toInt)
          val left = line.toVector.get(x - 1).map(_.toString.toInt)
          val right = line.toVector.get(x + 1).map(_.toString.toInt)

          List(up, down, left, right).flatten.forall(_ > digit)
        }
        if floor
      } yield digit

      digits.map(_ + 1).sum
    }
  }

  override val testOutput2: Long = 1134
  override def task2(input: Stream[IO, String]): IO[Long] = {
    input.compile.toVector map { vector =>
      case class Position(x: Int, y: Int, value: Int) {
        def top: Option[Position] = Position.resolve(x, y - 1)
        def down: Option[Position] = Position.resolve(x, y + 1)
        def left: Option[Position] = Position.resolve(x - 1, y)
        def right: Option[Position] = Position.resolve(x + 1, y)
        def neighbours: List[Position] = List(top, down, left, right).flatten
        def isLowPoint: Boolean = neighbours.forall(_.value > value)
      }
      object Position {
        def resolve(x: Int, y: Int): Option[Position] =
          for {
            line <- vector.get(y)
            digit <- line.toVector.get(x)
          } yield Position(x, y, digit.toString.toInt)
      }
      val sizes = for {
        y <- vector.indices
        line = vector(y)
        x <- line.indices
        position = Position(x, y, line.charAt(x).toString.toInt)
        if position.isLowPoint
      } yield {
        def explore(acc: Set[Position], p: Position): Set[Position] = {
          if (acc(p)) acc
          else {
            p.neighbours
              .filter(_.value >= p.value)
              .filter(_.value =!= 9)
              .foldLeft(acc + p)(explore)
          }
        }
        explore(Set.empty, position).size
      }

      sizes.sorted.reverse.take(3).product
    }
  }
}
