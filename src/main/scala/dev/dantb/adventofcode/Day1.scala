package dev.dantb
package adventofcode

import scala.io.Source

object Day1:
  def solvePart1: Int = compute(readFromFile)
  def solvePart2: Int = computePart2(readFromFile)

  def readFromFile: List[Int] = Utils.readFromFile("day1-aoc.txt").map(_.toInt)

  def compute(input: List[Int]): Int =
    input
      .foldLeft((0, input(0))) { case ((sum, previous), next) =>
        if next > previous then (sum + 1, next) else (sum, next)
      }
      ._1

  def computePart2(input: List[Int]): Int =
    val slidingWindowSize = 3
    input.sliding(slidingWindowSize + 1).count(window => window(slidingWindowSize) > window(0))

  def solveAlt: Int = altSolution(readFromFile)
  def altSolution(input: List[Int]): Int =
    // Array might be better?
    val size   = input.size
    var sum    = 0
    var idx    = 3
    var first  = input(0)
    var second = input(1)
    var third  = input(2)
    var window = first + second + third
    while idx < size do
      val next      = input(idx)
      val newWindow = (window - first) + next
      if newWindow > window then sum = sum + 1
      first = second
      second = third
      third = next
      window = newWindow
      idx = idx + 1
    sum
