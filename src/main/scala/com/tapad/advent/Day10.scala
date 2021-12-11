package com.tapad.advent

import cats.implicits._
import cats.effect.IO
import fs2._

import scala.annotation.tailrec
import scala.collection.immutable.Stack

object Day10 extends AventDay {
  override val filename: String = "day10.txt"
  override val testInput: Stream[IO, String] = Stream(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  )

  case class Pair(open: Char, close: Char)
  type Stack = List[Char]
  type Result = Either[Char, Stack]
  val pairs = Set(
    Pair('(', ')'),
    Pair('[', ']'),
    Pair('{', '}'),
    Pair('<', '>')
  )
  val opens: Map[Char, Char] = pairs.map(p => p.open -> p.close).toMap

  val corruptPoints: Map[Char, Long] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val autoCompletePoints: Map[Char, Long] = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  def parse: Stream[IO, String] => Stream[IO, Result] = _.map { line =>
    @tailrec
    def go(stack: Stack, text: List[Char]): Either[Char, Stack] = {
      text match {
        case head :: tail =>
          opens.get(head) match {
            case Some(close) =>
              go(close :: stack, tail)
            case None =>
              if (stack.head === head) go(stack.tail, tail)
              else Left(head)
          }
        case Nil => Right(stack)
      }
    }
    go(Nil, line.toList)
  }

  override val testOutput1: Long = 26397
  override def task1(input: Stream[IO, String]): IO[Long] =
    input
      .through(parse)
      .map(_.fold(corruptPoints, _ => 0L))
      .compile.foldMonoid

  override val testOutput2: Long = 288957
  override def task2(input: Stream[IO, String]): IO[Long] = {
    def score: Stack => Long = _.foldLeft(0L) { (acc, c) =>
      acc * 5 + autoCompletePoints(c)
    }

    input
      .through(parse)
      .map(_.fold(_ => 0L, score))
      .compile.toList map { scores =>
        val sorted = scores.filter(_ > 0).toVector.sorted
        sorted(sorted.length / 2)
      }
  }

}
