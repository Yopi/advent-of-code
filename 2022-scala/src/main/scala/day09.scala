package org.adventofcode.day09

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

@main def day09(): Unit =
  solve(readFile("input/day09_example"))
  println("--")
  solve(readFile("input/day09_example2"))
  println("--")
  solve(readFile("input/day09"))

def solve(input: String): Unit =
  val part1 = solvePart1(input)
  println(s"Part 1: ${part1}")

  val part2 = solvePart2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

def printRope(direction: String, steps: Integer, rope: Rope): Unit =
  println(s"== ${direction} ${steps} ==")
  println("")
  for (y <- -10 to 10) {
    for (x <- -10 to 10) {
      if (rope.position == (x, y)) {
        print("H")
      } else if (rope.tailPositions.contains((x, y))) {
        var printed = false
        for (i <- rope.tailPositions.indices) {
          if (rope.tailPositions(i) == (x, y) && !printed) {
            print(i + 1)
            printed = true
          }
        }
      } else if ((x, y) == (0, 0)) {
        print("s")
      } else {
        print(".")
      }
    }
    println("")
  }
  println("")
  println("")

class Rope(length: Int):
  var position: (Int, Int) = (0, 0) // x, y
  var tailPositions: List[(Int, Int)] = (1 to length).map(_ => (0, 0)).toList
  var tailVisited: Map[(Int, Int), Boolean] = Map()

  def move(direction: String, steps: Int): Unit =
    (1 to steps).foreach(_ => {
      // printRope(direction, steps, this)

      direction.match {
        case "U" => position = (position._1, position._2 - 1)
        case "D" => position = (position._1, position._2 + 1)
        case "L" => position = (position._1 - 1, position._2)
        case "R" => position = (position._1 + 1, position._2)
      }
      for (i <- this.tailPositions.indices) {
        val (x1, y1) = if (i == 0) position else this.tailPositions(i - 1)
        val (x2, y2) = this.tailPositions(i)
        val xDiff = Math.abs(x1 - x2)
        val yDiff = Math.abs(y1 - y2)

        val xInc = if (x1 > x2) 1 else if (x1 < x2) -1 else 0
        val yInc = if (y1 > y2) 1 else if (y1 < y2) -1 else 0

        if (xDiff > 1 || yDiff > 1) {
          this.tailPositions = this.tailPositions.updated(i, (x2 + xInc, y2 + yInc))
        }
      }
      this.tailVisited = tailVisited + (this.tailPositions.last -> true)
    })

def solvePart1(input: String): Int =
  val parsed = input.lines()
    .map(l => l.splitAt(1))
    .map(l => (l._1, l._2.trim.toInt))
    .toList

  val rope = Rope(1)
  parsed.forEach((dir, step) => rope.move(dir, step))
  rope.tailVisited.values.count(_ => true)


def solvePart2(input: String): Int =
  val parsed = input.lines()
    .map(l => l.splitAt(1))
    .map(l => (l._1, l._2.trim.toInt))
    .toList

  val rope = Rope(9)
  parsed.forEach((dir, step) => rope.move(dir, step))
  rope.tailVisited.values.count(b => true)
