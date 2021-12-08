package dev.dantb
package adventofcode

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day5:
  case class Input(lines: List[Line]):
    def ::(line: Line) = Input(line :: lines)

  case class Position(x: Int, y: Int)
  case class Line(start: Position, end: Position):
    def isHorizontal: Boolean = start.y == end.y
    def isVertical: Boolean   = start.x == end.x
    def isDiagonal: Boolean   = !isHorizontal && !isVertical

  def solve: Int       = compute(Parsing.parseInput(Utils.readFromFile("day5-aoc.txt")))
  def solvePart2: Int  = computePart2(Parsing.parseInput(Utils.readFromFile("day5-aoc.txt")))
  def solveSample: Int = computePart2(Parsing.parseInput(Utils.readFromFile("day5-aoc-sample.txt")))

  def compute(input: Input): Int =
    generatePositionCounts(input.lines.filterNot(_.isDiagonal)).count((_, count) => count >= 2)

  def computePart2(input: Input): Int =
    generatePositionCounts(input.lines).count((_, count) => count >= 2)

  def generatePositionCounts(lines: List[Line]): Map[Position, Int] =
    lines.foldLeft(Map.empty) { (positionToCount, line) =>
      positionsTouchedBy(line).foldLeft(positionToCount)((currentMap, pos) =>
        currentMap.updatedWith(pos)(_.map(_ + 1).orElse(Some(1)))
      )
    }

  def positionsTouchedBy(line: Line): List[Position] =
    if line.isHorizontal then generateBetween(line.start.x, line.end.x).map(Position(_, line.start.y))
    else if line.isVertical then generateBetween(line.start.y, line.end.y).map(Position(line.start.x, _))
    else positionsTouchedByDiagonal(line)

  def positionsTouchedByDiagonal(line: Line): List[Position] =
    val yStep: Int = if line.start.y < line.end.y then 1 else -1
    generateBetween(line.start.x, line.end.x)
      .foldLeft((List.empty[Position], line.start.y)) { case ((acc, y), x) =>
        (Position(x, y) :: acc, y + yStep)
      }
      ._1

  def generateBetween(start: Int, end: Int): List[Int] =
    generateBetween(start, end, if start < end then 1 else -1).toList

  def generateBetween(start: Int, end: Int, step: Int): List[Int] = (start to end by step).toList

  def printMap(map: Map[Position, Int]): Unit =
    val prettyMap: String =
      (0 to 9)
        .map(row =>
          (0 to 9)
            .map(col => map.get(Position(col, row)).map(_.toString).getOrElse("."))
            .mkString("")
        )
        .mkString("\n")
    println(prettyMap)

  object Parsing:
    def parseInput(input: List[String]): Input =
      input.foldLeft(Input(Nil))((acc, line) => parseLine(line) :: acc)

    def parseLine(raw: String): Line = raw.split(" -> ").map(parsePosition) pipe { line =>
      Line(line(0), line(1))
    }
    def parsePosition(raw: String) = raw.split(',') pipe { pos =>
      Position(pos(0).toInt, pos(1).toInt)
    }
