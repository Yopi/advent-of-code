package org.adventofcode.day07

import scala.:+
import scala.io.Source
import scala.util.Using

@main def day07(): Unit =
  solve(readFile("input/day07_example"))

  solve(readFile("input/day07"))

def solve(input: String): Unit =
  val part1 = solvePart1(input)
  println(s"Part 1: ${part1}")

  val part2 = solvePart2(input)
  println(s"Part 2: ${part2}")

def readFile(inputFile: String): String =
  Using(Source.fromFile(inputFile)) {
    _.mkString
  }.get

def parseDirectory(currentFolder: String, go: String): String =
  go.match {
    case "/" => "/"
    case ".." => currentFolder.splitAt(currentFolder.lastIndexOf("/"))._1
    case _ => currentFolder + "/" + go
  }

class Folder(name: String):
  var folders: List[Folder] = List[Folder]()
  var size: Int = 0

  def addSize(inc: Int): Unit = size = size + inc

  def getSize: Int = size + folders.map(_.getSize).sum

  def addFolder(folder: Folder): Unit = folders = folders :+ folder

def parseFolders(input: String): scala.collection.mutable.Map[String, Folder] =
  val cdCmd = "\\$ cd ?(.*)?".r
  val lsCmd = "\\$ ls".r
  val dir = "dir (.+)".r
  val file = "([0-9]+) (.+)".r

  var currentFolder = "/"
  val folders = scala.collection.mutable.Map[String, Folder]()
  input
    .lines()
    .forEachOrdered {
      case cdCmd(dir) => {
        currentFolder = parseDirectory(currentFolder, dir)
        if (!folders.contains(currentFolder)) {
          folders += (currentFolder -> Folder(currentFolder))
        }
      }
      case dir(file) => {
        val dirToFile = parseDirectory(currentFolder, file)
        val current = folders(currentFolder)
        if (!folders.contains(dirToFile)) {
          val folder = Folder(currentFolder)
          folders += (dirToFile -> folder)
          current.addFolder(folder)
        } else {
          val folder = folders(dirToFile)
          current.addFolder(folder)
        }
        folders.put(currentFolder, current)
      }
      case file(size, name) => {
        folders(currentFolder).addSize(size.toInt)
      }
      case lsCmd => // Do nothing
    }

  folders

def solvePart1(input: String): Int =
  parseFolders(input)
    .values
    .filter(f => f.getSize < 100_000)
    .map(_.getSize)
    .sum

def solvePart2(input: String): Int =
  val totalSpace = 70_000_000
  val updateReq = 30_000_000
  val folders = parseFolders(input)
  val unusedSpace = totalSpace - folders("/").getSize
  val freeNeeded = updateReq - unusedSpace
  folders
    .values
    .filter(f => f.getSize > freeNeeded)
    .map(_.getSize)
    .min
