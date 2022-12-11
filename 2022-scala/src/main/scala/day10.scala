package org.adventofcode.day10

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

@main def day10(): Unit =
  solve(readFile("input/day10_example2"))
  solve(readFile("input/day10"))

def solve(input: String): Unit =
  val part1 = solvePart1(input)
  println(s"Part 1: ${part1}")

  val part2 = solvePart2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

case class Command(val cycle: Int, val register: Int)

def parseInput(input: String): List[Command] =
  val addx = "addx (-?[0-9]+)".r
  input
    .split("\n").flatMap {
    case "noop" => List(Command(1, 0))
    case addx(num) => List(Command(1, 0), Command(1, num.toInt))
  }
    .toList

def scanCommands(res: (Int, Int), cmd: Command) =
  val (reg: Int, cycle: Int) = res
  (reg + cmd.register, cycle + cmd.cycle)

def solvePart1(input: String): Int =
  val seed = (1, 1)
  parseInput(input)
    .scanLeft(seed)(scanCommands)
    .filter((reg, cycle) => List(20, 60, 100, 140, 180, 220).contains(cycle))
    .map((reg, cycle) => reg * (if (cycle < 200) cycle else 220))
    .sum

def solvePart2(input: String): String =
  val seed = (1, 1)
  "\n" + parseInput(input)
    .scanLeft(seed)(scanCommands)
    .map((reg, cycle) => if (List(reg - 1, reg, reg + 1).contains((cycle - 1) % 40)) "█" else " ")
    .grouped(40)
    .map(l => l.mkString(""))
    .mkString("\n")
