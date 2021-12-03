package dev.dantb
package adventofcode

import scala.io.Source

object Day3:

  def solve = computePart1(readFromFile)

  def readFromFile: List[String] =
    val source = Source.fromResource("day3-aoc.txt")
    val lines = source.getLines.toList
    source.close
    lines

  def test = computePart1(List("1001", "0001", "1111"))

  def computePart1(input: List[String]): Int =
    def ones(numWithIdx: List[(Char, Int)], acc: Map[Int, Int]): Map[Int, Int] = numWithIdx match
      case Nil => acc
      case (c, idx) :: cs => 
        // println(s"Next is $c and index $idx")
        if c == '1' then ones(cs, acc.updatedWith(idx)(_.map(_ + 1).orElse(Option(1))))
        else ones(cs, acc.updatedWith(idx)(_.map(identity).orElse(Option(0))))

    val onesByIndex: Map[Int, Int] = input.map(_.toList.zipWithIndex).foldLeft(Map.empty[Int, Int]) { case (acc, num) =>
      ones(num, acc)
    }

    val halfInputLength = input.size / 2
    val gammaString = onesByIndex.toList.sortBy(_._1).map { (_, count) => 
      if count > halfInputLength then '1' else '0' 
    }.mkString

    val gamma = Integer.valueOf(gammaString, 2)
    val epsilon = Integer.valueOf(not(gammaString), 2)
    
    gamma * epsilon

  def not(str: String): String = str.foldLeft("")((acc, next) => if next == '1' then acc :+ '0' else acc :+ '1')

  // def computePart2(input: List[String]): Int =
  //   def ones(numWithIdx: List[(Char, Int)], acc: Map[Int, Int]): Map[Int, Int] = numWithIdx match
  //     case Nil => acc
  //     case (c, idx) :: cs => 
  //       // println(s"Next is $c and index $idx")
  //       if c == '1' then ones(cs, acc.updatedWith(idx)(_.map(_ + 1).orElse(Option(1))))
  //       else ones(cs, acc.updatedWith(idx)(_.map(identity).orElse(Option(0))))

  //   val onesByIndex: Map[Int, Int] = input.map(_.toList.zipWithIndex).foldLeft(Map.empty[Int, Int]) { case (acc, num) =>
  //     val newMap = ones(num, acc)
  //     // println(s"New map $newMap")
  //     newMap
  //   }

  //   val inputLength = input.size
  //   val halfInputLength = input.size / 2
  //   // val binaryLength = input.headOption.map(_.size).getOrElse(0)
  //   // val halfBinaryLength = binaryLength / 2 // could it be odd? can they be the same?
  //   println(s"Input length $inputLength")
  //   println(s"Half input length $halfInputLength")
  //   val gammaRaw = onesByIndex.toList.sortBy(_._1).map { (_, count) => 
  //     if count == halfInputLength then println(s"SAME COUNT FOR BOTH!")
  //     if count > halfInputLength then '1' else '0' 
  //   }.mkString

  //   val gamma = Integer.valueOf(gammaRaw, 2)

  //   println(s"Gamma string is $gammaRaw")
  //   println(s"Gamma is $gamma")

  //   val epsilonString = not(gammaRaw)
  //   val epsilon = Integer.valueOf(epsilonString, 2)

  //   println(s"Epsilon string is $epsilonString")
  //   println(s"Epsilon is $epsilon")
    
  //   gamma * epsilon

