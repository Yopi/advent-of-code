package org.adventofcode.day05

import scala.io.Source
import scala.util.Using

@main def day05(): Unit =
  solve("input/day05_example")

  solve("input/day05")

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

def parseStacks(str: String): Map[Int, List[String]] =
  str
    .replaceAll(" {4}", "[_] ")
    .replaceAll("\\[", "")
    .replaceAll("]", "")
    .replaceAll(" ", "")
    .split("\n")
    .dropRight(1)
    .map(x => x.split(""))
    .map(x => x
      .zipWithIndex
      .filterNot(p => p._1 == "_")
      .map(_.swap).toSeq)
    .reduce((x, y) => x ++ y)
    .groupBy(_._1)
    .transform((k, v) => v.map(_._2).toList)

def parseInstructions(str: String): Array[(Int, Int, Int)] =
  str
    .split("\n")
    .map(x =>
    val move = "move (\\d+) from (\\d+) to (\\d+)".r
    x.match {
      case move(n, f, t) => (n.toInt, f.toInt, t.toInt)
    })

def parseInput(input: String): (Map[Int, List[String]], (Array[(Int, Int, Int)])) =
  val i = input
    .split("\n\n")
    .match { case Array(a, b) => (a, b) }

  val stacks = parseStacks(i._1)
  val instructions = parseInstructions(i._2)

  (stacks, instructions)

def solveP1(input: String): String =
  var (stacks, instructions) = parseInput(input)

  instructions.foreach((n, f, t) => {
    for (_ <- 1 to n) {
      val stackFrom = stacks(f - 1)
      val stackTo = stacks(t - 1)
      val replacedStack: List[String] = List(stackFrom.head) ++ stackTo
      stacks = stacks.updated(f - 1, stackFrom.drop(1))
      stacks = stacks.updated(t - 1, replacedStack)
    }
  })

  (0 to stacks.keys.size).map((i) => stacks.getOrElse(i, List("")).head).mkString

def solveP2(input: String): String =
  var (stacks, instructions) = parseInput(input)

  instructions.foreach((n, f, t) => {
    val stackFrom = stacks(f - 1)
    val stackTo = stacks(t - 1)
    val replacedStack: List[String] = stackFrom.take(n) ++ stackTo
    stacks = stacks.updated(f - 1, stackFrom.drop(n))
    stacks = stacks.updated(t - 1, replacedStack)
  })

  (0 to stacks.keys.size).map((i) => stacks.getOrElse(i, List("")).head).mkString
