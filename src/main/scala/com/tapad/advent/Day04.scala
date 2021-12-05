package com.tapad.advent

import cats.effect.IO
import fs2.Stream

object Day04 extends AventDay {
  override val filename: String = "day04-full.txt"
  override val testInput: Stream[IO, String] = readFile("day04-test.txt")

  case class Board(sets: List[Set[Int]], draws: List[Int]) {
    val drawsRequired: List[Int] = {
      (1 to draws.length)
        .map(draws.take)
        .find { items =>
          val set = items.toSet
          sets.exists(_.forall(set))
        }
        .get
    }

    val drawSizeRequired: Int = drawsRequired.size

    def score: Int = {
      val unmarked = (sets.flatten.toSet diff drawsRequired.toSet).sum
      drawsRequired.last * unmarked
    }
  }

  override val testOutput1: Long = 4512
  override def task1(input: Stream[IO, String]): IO[Long] = {
    parseBoards(input) map { boards =>
      boards.minBy(_.drawSizeRequired).score
    }
  }

  override val testOutput2: Long = 1924
  override def task2(input: Stream[IO, String]): IO[Long] = {
    parseBoards(input) map { boards =>
      boards.maxBy(_.drawSizeRequired).score
    }
  }

  def parseBoards(input: Stream[IO, String]): IO[List[Board]] = {
    input.compile.toList map {
      case head :: tail =>
        val draws = head.split(",").toList.map(_.toInt)

        tail.grouped(6).toList map { lines =>
          val ints = lines.drop(1).map(_.split(" ").toList.filter(_.nonEmpty).map(_.toInt))

          val sets =
            ints.map(_.toSet) ++ (0 until 5).map { index =>
              ints.map(_.apply(index)).toSet
            }
          Board(sets, draws)
        }
    }
  }
}
