package org.adventofcode.day06

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

@main def day06(): Unit =

  solve("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
  solve("bvwbjplbgvbhsrlpgdmjqwftvncz")
  solve("nppdvjthqldpwncqszvftbrmjlhg")
  solve("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  solve("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

  println("--")
  solve(readFile("input/day06"))

def solve(input: String): Unit =
  val part1 = solveParts(input, 4)
  println(s"Part 1: ${part1}")

  val part2 = solveParts(input, 14)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

def isUnique(lst: List[Char])=
  lst.distinct.size == lst.size

def solveParts(input: String, som: Int): Int =
  var chars = List[Char]()
  var idx = 0
  input.zipWithIndex.iterator.takeWhile(_ => idx == 0).foreach { (elem, i) =>
    chars = chars :+ elem
    if (i >= som) {
      if (isUnique(chars.takeRight(som))) {
        idx = i+1
      }
    }
  }

  idx
