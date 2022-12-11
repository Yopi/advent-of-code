package org.adventofcode.day11

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

@main def day11(): Unit =
  solve(readFile("input/day11_example"))
  solve(readFile("input/day11"))

def solve(input: String): Unit =
  val part1 = solvePart1(input)
  println(s"Part 1: ${part1}")

  val part2 = solvePart2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

class Monkey(var items: List[Int], val operation: String, val test: Int, val ifTrue: Int, val ifFalse: Int):
  var inspections: Int = 0

  def addItem(newItem: Int): Unit =
    this.items = this.items :+ newItem

  def inspect(item: Int, decrease: BigInt => Int): (Int, Int) =
    inspections = inspections + 1
    val newWorryLevel = decrease(conductOperation(BigInt(item)))
    (newWorryLevel, if (newWorryLevel % test == 0) ifTrue else ifFalse)

  def clearItems(): Unit =
    this.items = List()

  private def conductOperation(worryLevel: BigInt): BigInt =
    this.operation match
      case s"old + $b" => if (b == "old") worryLevel + worryLevel else worryLevel + b.toInt
      case s"old * $b" => if (b == "old") worryLevel * worryLevel else worryLevel * b.toInt

def parseInput(input: String): List[Monkey] =
  input
    .split("\n\n")
    .map(monkey => monkey
      .split("\n")
      .map {
        case s"Monkey $m:" => ("id" -> m)
        case s"  Starting items: $i" => ("items" -> i)
        case s"  Operation: new = $op" => ("operation" -> op)
        case s"  Test: divisible by $test" => ("test" -> test)
        case s"    If true: throw to monkey $newMonkey" => ("ifTrue" -> newMonkey)
        case s"    If false: throw to monkey $newMonkey" => ("ifFalse" -> newMonkey)
      }.toMap[String, String]
    )
    .map(m => Monkey(
      m("items").split(", ").map(_.toInt).toList,
      m("operation"),
      m("test").toInt,
      m("ifTrue").toInt,
      m("ifFalse").toInt
    )).toList

def solveParts(monkeys: List[Monkey], rounds: Int, decreaseFunc: BigInt => Int): BigInt =
  (1 to rounds).foreach(_ =>
    monkeys.foreach(m => {
      m.items.map(i => m.inspect(i, decreaseFunc)).foreach((newWorryLevel, monkey) => monkeys(monkey).addItem(newWorryLevel))
      m.clearItems()
    })
  )
  monkeys.map(m => m.inspections).map(i => BigInt(i)).sorted.takeRight(2).product


def solvePart1(input: String): Int =
  val monkeys = parseInput(input)
  solveParts(monkeys, 20, (i: BigInt) => (i / 3).toInt).toInt

def solvePart2(input: String): BigInt =
  val monkeys = parseInput(input)
  val leastCommonMultiplier = monkeys.map(m => m.test).product
  solveParts(monkeys, 10_000, (i: BigInt) => (i % leastCommonMultiplier).toInt)
