package org.adventofcode

import scala.io.Source
import scala.util.Using

@main def day02(): Unit =
  solve("input/day02_example")

  solve("input/day02")

def solve(filename: String): Unit =
  val input = readFile(filename)

  val part1 = solveP1(input)
  println(s"Part 1: ${part1}")

  val part2 = solveP2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) { _.mkString }.get

def parseInput(input: String): Array[Array[String]] =
  input
    .split("\n")
    .map(x => x.split(" "))

def solveP1(input: String): Int =
  parseInput(input)
    .map {
      case Array("A", "X") => (1 + 3)
      case Array("A", "Y") => (2 + 6)
      case Array("A", "Z") => (3)
      case Array("B", "X") => (1 + 0)
      case Array("B", "Y") => (2 + 3)
      case Array("B", "Z") => (3 + 6)
      case Array("C", "X") => (1 + 6)
      case Array("C", "Y") => (2)
      case Array("C", "Z") => (3 + 3)
      case _ => 0
    }.sum


def solveP2(input: String): Int =
  parseInput(input)
    .map {
      case Array("A", "X") => (3)
      case Array("A", "Y") => (1 + 3)
      case Array("A", "Z") => (2 + 6)
      case Array("B", "X") => (1)
      case Array("B", "Y") => (2 + 3)
      case Array("B", "Z") => (3 + 6)
      case Array("C", "X") => (2)
      case Array("C", "Y") => (3 + 3)
      case Array("C", "Z") => (1 + 6)
      case _ => 0
    }.sum
