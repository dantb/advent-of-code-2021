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

  def solveAlt: Int = altSolution(readFromFile)
  def altSolution(input: List[Int]): Int =
    // Array might be better?
    val size = input.size
    var sum = 0
    var idx = 3
    var first = input(0)
    var second = input(1)
    var third = input(2)
    var window = first + second + third
    while (idx < size)
      val next = input(idx)
      val newWindow = (window - first) + next
      if newWindow > window then sum = sum + 1
      first = second
      second = third
      third = next
      window = newWindow
      idx = idx + 1
    sum