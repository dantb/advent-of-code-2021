package dev.dantb
package adventofcode

import scala.io.Source
import scala.math
import scala.collection.immutable.Vector
import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day6:
  def solve: Long      = compute(Parsing.parseInput(Utils.readFromFile("day6.txt")), 80)
  def solvePart2: Long = compute(Parsing.parseInput(Utils.readFromFile("day6.txt")), 256)
  def solveSample: Long =
    compute(Parsing.parseInput(Utils.readFromFile("day6-sample.txt")), 80)

  def compute(input: List[Int], totalDays: Int): Long =
    val buffer: Array[Long] = Array.ofDim(9)

    input.groupBy(identity).map((timer, list) => (timer, list.size)).foreach { (timer, count) =>
      buffer(timer) = count
    }

    @tailrec
    def loop(daysLeft: Int): Long =
      if daysLeft <= 0 then buffer.sum
      else
        val oldZero = buffer(0)
        (0 to 5).foreach { i =>
          buffer(i) = buffer(i + 1)
        }
        buffer(6) = buffer(7) + oldZero
        buffer(7) = buffer(8)
        buffer(8) = oldZero
        loop(daysLeft - 1)
    loop(totalDays)

// Naive solution. Feels like there's a smart solution using exponential growth formula?
// def computeNaively(input: List[Int], totalDays: Int): Long =
//   def loop(fishTimers: Vector[(Int, Int)], acc: Long): Long =
//     if fishTimers.isEmpty then acc
//     else
//       val daysNewFishedProducedOn = fishTimers.foldLeft(Vector.empty[(Int, Int)]) { case (acc, (fishTimer, dayProducedOn)) =>
//         val newFishProducedOn = daysFishProducedOn(fishTimer, totalDays - dayProducedOn, ProductionRate)
//         acc.appendedAll(newFishProducedOn.map { day => (dayProducedOn + NewFishProductionRate - 1, day) })
//       }
//       loop(daysNewFishedProducedOn, acc + fishTimers.size)
//   loop(input.map(x => (x, 0)).toVector, 0L)

// def computeFishYield(fishTimer: Int, daysLeft: Int, productionRate: Int): Int =
//   math.floorDiv(daysLeft - fishTimer - 1, productionRate) + 1

// def daysFishProducedOn(fishTimer: Int, daysLeft: Int, productionRate: Int): IndexedSeq[Int] =
//   val fishYield = computeFishYield(fishTimer, daysLeft, productionRate)
//   (0 until fishYield).map(nthFish => (fishTimer + 1) + (nthFish * productionRate))

object Parsing:
  def parseInput(input: List[String]): List[Int] =
    input(0).split(",").map(_.toInt).toList
