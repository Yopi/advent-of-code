package day01

import scala.io.Source
import scala.util.Using

@main def day01(): Unit =
  part1("input/day01_example")

  part1("input/day01")

def part1(filename: String): Unit =
  val input = readFile(filename)

  val part1 = solve(input)
  println(s"Part 1: ${part1}")

  val part2 = solve(input, 3)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) { _.mkString }.get

def parseInput(input: String): Array[Int] =
  input
    .split("\n\n")
      .map((e) => e.split("\n")
          .map((l) => l.toInt)
        .sum)
    .sortWith(_ > _)

def solve(input: String, taken: Int = 1): Int = parseInput(input).take(taken).sum