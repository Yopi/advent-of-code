package org.adventofcode.day12

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

@main def day12(): Unit =
  solve(readFile("input/day12_example"))
// solve(readFile("input/day11"))

def solve(input: String): Unit =
  val part1 = solvePart1(input)
  println(s"Part 1: ${part1}")

// val part2 = solvePart2(input)
// println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

def convertCharToHeight(char: String): Int =
  char.match {
    case "S" => 0
    case "E" => 25
    case default => default.charAt(0).toInt - 97
  }

def parseInput(input: String): ((Int, Int), (Int, Int), Map[(Int, Int), Int]) =
  var start = (0, 0)
  var end = (0, 0)

  val m = input
    .split("\n")
    .zipWithIndex
    .flatMap((l, y) => l
      .split("")
      .zipWithIndex
      .map((c, x) => {
        if (c == "S") {
          start = (x, y)
        } else if (c == "E") {
          end = (x, y)
        }
        (x, y) -> convertCharToHeight(c)
      }
      )).toMap[(Int, Int), Int]

  (start, end, m)


def solvePart1(input: String): Unit =
  val (start, end, m) = parseInput(input)

