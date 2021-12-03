package com.tapad.advent.day01

import scala.io.Source

object PartOne extends App {
  val source = Source.fromURL(getClass.getResource("day01.txt"))
  val input =
    try source.getLines().toList
    finally source.close()

  val longs = input.map(_.toLong)

  val count = (longs zip longs.drop(1)).count { case (first, second) => first < second }

  println("Count: " + count)
}

object PartTwo {
  def main(args: Array[String]): Unit = {
    val source = Source.fromURL(getClass.getResource("day01.txt"))
    val input =
      try source.getLines().toList
      finally source.close()

    val windows = input.map(_.toLong).sliding(3).map(_.sum).toList
    val count = (windows zip windows.drop(1)).count { case (first, second) => first < second }

    println("Count: " + count)
  }
}