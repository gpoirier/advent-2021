package com.tapad.advent

import cats.implicits._
import cats.data.Chain
import cats.effect.IO
import com.tapad.advent.util.SimpleActionCache
import fs2.Stream

object Day06 extends AventDay {
  override val filename: String = "day06.txt"
  override val testInput: Stream[IO, String] = Stream("3,4,3,1,2")
  override val testOutput1: Long = 5934
  override val testOutput2: Long = 26984457539L

  override def task1(input: fs2.Stream[IO, String]): IO[Long] =
    input.compile.lastOrError.flatMap { line =>
      val ints = Chain.fromSeq(line.split(",")).map(_.toInt)

      val after = (1 to 80).foldLeft(ints) { (acc, _) =>
        nextDay(acc)
      }

      IO(after.size)
    }

  def nextDay(timers: Chain[Int]): Chain[Int] = {
    val newTimers = timers.foldLeft(Timers(Chain.empty, Chain.empty)) { (acc, current) =>
      if (current == 0) acc.resetNext
      else acc.withNext(current - 1)
    }
    newTimers.build
  }

  override def task2(input: Stream[IO, String]): IO[Long] = {
    for {
      line <- input.compile.lastOrError
      cache <- SimpleActionCache.make[IO, (Int, Int), Long]
      counts <- {
        def countFishes(initial: Int, days: Int): IO[Long] = {
          cache.get(initial -> days) {
            IO.defer {
              val nextChild = days - initial - 1
              if (nextChild < 0) 1L.pure[IO]
              else {
                val forSelf = countFishes(6, nextChild)
                val forChild = countFishes(8, nextChild)
                (forSelf, forChild).mapN(_ + _)
              }
            }
          }
        }

        val ints = Chain.fromSeq(line.split(",")).map(_.toInt)
        ints.traverse { initial =>
          countFishes(initial, 256)
        }
      }
    } yield counts.toList.sum
  }
}

case class Timers(acc: Chain[Int], additional: Chain[Int]) {
  def resetNext: Timers = Timers(acc :+ 6, additional :+ 8)
  def withNext(value: Int): Timers = Timers(acc :+ value, additional)

  def build: Chain[Int] = acc ++ additional
}
