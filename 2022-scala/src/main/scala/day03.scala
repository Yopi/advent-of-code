package org.adventofcode.day03

import scala.io.Source
import scala.util.Using

@main def day03(): Unit =
  solve("input/day03_example")

  solve("input/day03")

def solve(filename: String): Unit =
  val input = readFile(filename)

  val part1 = solveP1(input)
  println(s"Part 1: ${part1}")

  val part2 = solveP2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) { _.mkString }.get

def parseInput(input: String): Array[(String, String)] =
  input
    .split("\n")
    .map(x => x.splitAt(x.length/2))

def score (a: String): Int =
  val ch = a.charAt(0).intValue
  if (ch >= 97) {
    return ch - 96
  }
  ch - 64 + 26

def solveP1(input: String): Int =
  input
    .split("\n")
    .map(x => x.splitAt(x.length / 2))
    .map({ case (a, b) => a.intersect(b) })
    .map(score)
    .sum

def solveP2(input: String): Int =
  input
    .split("\n")
    .grouped(3)
    .map(a => a.fold(a.head) {(a, b) => a.intersect(b) })
    .map(score)
    .sum
