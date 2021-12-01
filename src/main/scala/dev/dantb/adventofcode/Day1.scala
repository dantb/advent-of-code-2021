package dev.dantb
package adventofcode

import scala.io.Source

object Day1:
  def solvePart1: Int = compute(readFromFile)
  def solvePart2: Int = computePart2(readFromFile)

  def readFromFile: List[Int] =
    val source = Source.fromResource("day1-aoc.txt")
    val lines = source.getLines.toList.map(_.toInt)
    source.close
    lines

  def compute(input: List[Int]): Int =
    input
      .foldLeft((0, input(0))) {
        case ((sum, previous), next) =>
          if next > previous then (sum + 1, next) else (sum, next)
      }
      ._1

  def computePart2(input: List[Int]): Int =
    input
      .drop(3)
      .foldLeft((0, input.take(3).sum, input(0), input(1), input(2))) {
        case ((sum, prevSum, minus3, minus2, minus1), next) =>
          val newSum = (prevSum + next) - minus3
          if newSum > prevSum then (sum + 1, newSum, minus2, minus1, next)
          else (sum, newSum, minus2, minus1, next)
      }
      ._1
