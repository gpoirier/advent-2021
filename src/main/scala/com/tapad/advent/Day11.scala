package com.tapad.advent

import cats.implicits._
import cats.effect.IO
import fs2.Stream

object Day11 extends AventDay {
  override val filename: String = "day11-full.txt"
  override val testInput: Stream[IO, String] = readFile("day11-test.txt")

  type Grid = Vector[Vector[Int]]

  override val testOutput1: Long = 1656
  override def task1(input: Stream[IO, String]): IO[Long] =
    input.compile.toVector map { lines =>
      lines.map(_.toVector.map(_.toString.toInt))

      def step(grid: Grid): (Grid, Int) = {
        val m = grid.map(_.toBuffer).toBuffer
        def inc(i: Int, j: Int): Unit = {
          val row = m(i)
          val newValue = row(i) + 1
          row(i) = newValue
          if (newValue == 10) {

          }
        }
        for {
          i <- m.indices
          j <- m(i).indices
        } inc(i, j)
        ???
      }
      1
    }

  override val testOutput2: Long = 1
  override def task2(input: Stream[IO, String]): IO[Long] = IO(1)
}
