package org.adventofcode.day13

import org.json4s.{JArray, JInt, JValue}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import org.json4s.native.*

@main def day13(): Unit =

  solve(readFile("input/day13_example"))
  solve(readFile("input/day13"))

def solve(input: String): Unit =
  /*
  val part1 = solvePart1(input)
  println(s"Part 1: ${part1}")
  */

  val part2 = solvePart2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

def parsePacket(line: String): JValue =
  JsonMethods.parse(line)

def parseInput(input: String): List[List[JValue]] =
  input
    .split("\n\n")
    .map(_.split("\n", 2)
      .map(parsePacket)
      .toList
    ).toList

def parseInput2(input: String): List[JValue] =
  input
    .split("\n")
    .filterNot(n => n.isBlank)
    .map(parsePacket)
    .toList

enum Result:
  case Correct, Incorrect, Continue

def comp(a: JInt, b: JInt): Result =
  if (a.num < b.num) {
    Result.Correct
  } else if (a.num == b.num) {
    Result.Continue
  } else {
    Result.Incorrect
  }

def comp(a: List[JValue], b: List[JValue]): Result =
  if (a.isEmpty && b.isEmpty) {
    return Result.Continue
  }
  if (a.isEmpty) {
    return Result.Correct
  }
  if (b.isEmpty) {
    return Result.Incorrect
  }
  comp(a.head, b.head) match
    case Result.Correct => Result.Correct
    case Result.Incorrect => Result.Incorrect
    case Result.Continue => comp(a.tail, b.tail)

def comp(a: JValue, b: JValue): Result =
  val matched = (a, b) match
    case (a: JArray, b: JArray) => comp(a.children, b.children)
    case (a: JInt, b: JInt) => comp(a, b)
    case (a: JInt, b: JArray) => comp(List(a), b.children)
    case (a: JArray, b: JInt) => comp(a.children, List(b))
  matched

def compare(lsts: List[JValue]): Boolean =
  val left = lsts.head
  val right = lsts(1)

  val comparison = comp(left, right)
  println(s"[${comparison}] ${left} < ${right}")
  comparison match
    case Result.Correct => true
    case Result.Incorrect => false
    case Result.Continue => false

def solvePart1(str: String): Int =
  parseInput(str)
    .map(compare)
    .zipWithIndex
    .map((b, i) => if (b) i+1 else -1)
    .filter(p => p >= 0)
    .sum

def sorted(left: JValue, right: JValue): Boolean =
  val comparison = comp(left, right)
  comparison match
    case Result.Correct => true
    case Result.Incorrect => false
    case Result.Continue => false

def solvePart2(str: String): Int =
  val two = parsePacket("[[2]]")
  val six = parsePacket("[[6]]")
  val sortedInput = parseInput2(str)
    .appended(two)
    .appended(six)
    .sortWith(sorted)

  (sortedInput.indexOf(two) + 1) * (sortedInput.indexOf(six) + 1)
