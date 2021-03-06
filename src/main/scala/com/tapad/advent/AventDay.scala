package com.tapad.advent

import cats.syntax.eq._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.{Pipe, Stream, Chunk}
import fs2.io.file.{Files, Path}

trait AventDay extends IOApp {

  val filename: String
  val testInput: Stream[IO, String]
  val testOutput1: Long
  val testOutput2: Long

  def task1(input: Stream[IO, String]): IO[Long]
  def task2(input: Stream[IO, String]): IO[Long]

  override def run(args: List[String]): IO[ExitCode] = {
    val fileInput = readFile(filename)

    for {
      testResult1 <- task1(testInput)
      testResult2 <- task2(testInput)

      _ <- printResult("Test Task 1", testResult1, testOutput1)
      _ <- printResult("Test Task 2", testResult2, testOutput2)

      result1 <- task1(fileInput)
      result2 <- task2(fileInput)

      _ <- info("Task 1: " + result1)
      _ <- info("Task 2: " + result2)
    } yield ExitCode.Success
  }

  def readFile(filename: String): Stream[IO, String] = {
    val path = Path(s"src/main/resources/com/tapad/advent/${filename}")
    Files[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .through(partition)
  }

  def partition: Pipe[IO, String, String] = fs2.text.lines

  def coma: Pipe[IO, String, String] = _.repartition(chunk => Chunk.array(chunk.split(",")))

  def printResult(name: String, value: Long, expected: Long): IO[Unit] =
    info(s"$name: " +  value + " - " + (if (value === expected) "Success" else "Failure"))

  def info(text: String): IO[Unit] = IO(println("[info] " + text))
}