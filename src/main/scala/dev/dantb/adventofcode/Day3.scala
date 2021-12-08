package dev.dantb
package adventofcode

import scala.io.Source

object Day3:
  def solve            = computePart1(Utils.readFromFile("day3-aoc.txt"))
  def solvePart2       = computePart2(Utils.readFromFile("day3-aoc.txt"))
  def solvePart2Sample = computePart2(Utils.readFromFile("day3-aoc-sample.txt"))

  def computePart1(input: List[String]): Int =
    val gammaString = computeGammaString(input)
    val gamma       = Integer.valueOf(gammaString, 2)
    val epsilon     = Integer.valueOf(not(gammaString), 2)
    gamma * epsilon

  def computeGammaString(input: List[String]): String =
    def ones(numWithIdx: List[(Char, Int)], acc: Map[Int, Int]): Map[Int, Int] = numWithIdx match
      case Nil => acc
      case (c, idx) :: cs =>
        if c == '1' then ones(cs, acc.updatedWith(idx)(_.map(_ + 1).orElse(Option(1))))
        else ones(cs, acc.updatedWith(idx)(_.orElse(Option(0))))

    val onesByIndex: Map[Int, Int] =
      input.map(_.toList.zipWithIndex).foldLeft(Map.empty[Int, Int]) { (acc, num) =>
        ones(num, acc)
      }

    val halfInputLength = if input.size % 2 == 0 then input.size / 2 else (input.size + 1) / 2
    onesByIndex.toList
      .sortBy(_._1)
      .map((_, count) => if count >= halfInputLength then '1' else '0')
      .mkString

  def not(str: String): String =
    str.foldLeft("")((acc, next) => if next == '1' then acc :+ '0' else acc :+ '1')

  def computePart2(input: List[String]): Int =
    val oxygenRatingString: String =
      def loop(list: List[String], idx: Int): List[String] = list match
        case Nil         => list
        case head :: Nil => list
        case other =>
          val gamma   = computeGammaString(other)
          val current = gamma(idx)
          loop(list.filter(_(idx) == current), idx + 1)
      loop(input, 0).head
    val oxygenRating = Integer.valueOf(oxygenRatingString, 2)

    val epsilonString = not(computeGammaString(input))
    val epsilon       = Integer.valueOf(epsilonString, 2)
    val co2String =
      def loop(list: List[String], idx: Int): List[String] = list match
        case Nil         => list
        case head :: Nil => list
        case other =>
          val epsilon = not(computeGammaString(other))
          val current = epsilon(idx)
          loop(list.filter(_(idx) == current), idx + 1)
      loop(input, 0).head
    val co2Rating = Integer.valueOf(co2String, 2)

    oxygenRating * co2Rating
