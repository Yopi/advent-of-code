package org.adventofcode.day08

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

@main def day08(): Unit =
  solve(readFile("input/day08_example"))
  solve(readFile("input/day08"))

def solve(input: String): Unit =
  val part1 = solvePart1(input)
  println(s"Part 1: ${part1}")

  val part2 = solvePart2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

def parseInput(input: String): Map[(Int, Int), Int] =
  input.split("\n")
    .zipWithIndex.map((trees, y) =>
    trees
      .split("")
      .zipWithIndex
      .map((h, x) => (x, y) -> h.toInt))
    .flatten
    .toMap[(Int, Int), Int]

def determineVisible(hw: (Int, Int)
                     , trees: Map[(Int, Int), Int]
                     , tree: ((Int, Int), Int)
                    ): Boolean =
  val (h, w) = hw
  val ((x, y), height) = tree
  val left = (-1 until x).map(nX => trees.getOrElse((nX, y), -1)).count(z => z >= height)
  if (left == 0) {
    return true
  }
  val right = (x+1 to w+1).map(nX => trees.getOrElse((nX, y), -1)).count(z => z >= height)
  if (right == 0) {
    return true
  }
  val up = (-1 until y).map(nY => trees.getOrElse((x, nY), -1)).count(z => z >= height)
  if (up == 0) {
    return true
  }
  val down = (y+1 to h+1).map(nY => trees.getOrElse((x, nY), -1)).count(z => z >= height)
  if (down == 0) {
    return true
  }

  false

def solvePart1(input: String): Int =
    val height = input.lines().count().toInt
    val width = input.lines().findFirst().get().length

    val parsed = parseInput(input)
    parsed
      .map { case (k, h) => (k, determineVisible((height, width), parsed, (k, h))) }
      .values
      .count(f => f)

def determineScenic(hw: (Int, Int)
                     , trees: Map[(Int, Int), Int]
                     , tree: ((Int, Int), Int)
                    ): Int =
  val (h, w) = hw
  val ((x, y), height) = tree
  val left = (0 until x).reverse.map(nX => trees(nX, y)).takeWhile(x => x < height)
    ++ (0 until x).reverse.map(nX => trees(nX, y)).dropWhile(x => x < height).take(1)
  val right = (x+1 until w).map(nX => trees(nX, y)).takeWhile(x => x < height)
    ++ (x+1 until w).map(nX => trees(nX, y)).dropWhile(x => x < height).take(1)
  val up = (0 until y).reverse.map(nY => trees(x, nY)).takeWhile(x => x < height)
    ++ (0 until y).reverse.map(nY => trees(x, nY)).dropWhile(x => x < height).take(1)
  val down = (y+1  until h).map(nY => trees(x, nY)).takeWhile(x => x < height)
    ++ (y+1  until h).map(nY => trees(x, nY)).dropWhile(x => x < height).take(1)

  println(s"${(x, y)}: ${left} * ${right} * ${up} * ${down} = ${left.length * right.length * up.length * down.length}")
  left.length * right.length * up.length * down.length

def solvePart2(input: String): Int =
  val height = input.lines().count().toInt
  val width = input.lines().findFirst().get().length
  val parsed = parseInput(input)

  determineScenic((height, width), parsed, ((2, 3), 5))

  val x = parsed.map { case (k, h) => (k, determineScenic((height, width), parsed, (k, h))) }
  println(x)
  x.values.max