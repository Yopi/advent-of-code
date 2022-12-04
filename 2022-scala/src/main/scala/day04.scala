package org.adventofcode.day04

import scala.io.Source
import scala.util.Using

@main def day04(): Unit =
  solve("input/day04_example")

  solve("input/day04")

def solve(filename: String): Unit =
  val input = readFile(filename)

  val part1 = solveP1(input)
  println(s"Part 1: ${part1}")

  val part2 = solveP2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

def divideRanges(i: String): (Range, Range) =
  i.split(",")
    .map(x => x.split("-")
      .map(_.toInt).match {
      case Array(a, b) => (a to b)
    })
  .match {
    case Array(a, b) => (a, b)
  }

def parseInput(input: String): List[(Range, Range)] =
  input
    .split("\n")
    .map(divideRanges)
    .toList

def rangeFullOverlap(a: Range, b: Range): Boolean =
  (a.contains(b.start) && a.contains(b.end))
    || (b.contains(a.start) && b.contains(a.end))

def solveP1(input: String): Int =
  parseInput(input)
    .map({ case (a, b) => rangeFullOverlap(a, b) })
    .count(_ == true)

def rangeOverlap(a: Range, b: Range): Boolean =
  (a.contains(b.start) || a.contains(b.end))
    || (b.contains(a.start) || b.contains(a.end))

def solveP2(input: String): Int =
  parseInput(input)
    .map({ case (a, b) => rangeOverlap(a, b) })
    .count(_ == true)
