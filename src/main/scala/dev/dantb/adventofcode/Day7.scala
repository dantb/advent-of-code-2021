package dev.dantb
package adventofcode

import scala.io.Source
import scala.math
import scala.collection.immutable.Vector
import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day7:
  def solve: Long            = compute(Parsing.parseInput(Utils.readFromFile("day7.txt")))
  def solveSample: Long      = compute(Parsing.parseInput(Utils.readFromFile("day7-sample.txt")))
  def solvePart2: Long       = computePart2(Parsing.parseInput(Utils.readFromFile("day7.txt")))
  def solvePart2Sample: Long = computePart2(Parsing.parseInput(Utils.readFromFile("day7-sample.txt")))

  final case class CrabPosition(position: Int, numberOfCrabs: Int)

  def compute(input: List[Int]): Long =
    computeWithFunction(input, part1computeTotalFuelByPositionOneWay)

  def computePart2(input: List[Int]): Long =
    computeWithFunction(input, part2ComputeTotalFuelByPositionOneWay)

  def computeWithFunction(input: List[Int], totalFuelByPositionOneWay: List[CrabPosition] => Map[Int, Int]): Long =
    val crabCountByPosition: Map[Int, Int] = input.groupBy(identity).map((pos, list) => (pos, list.size))
    // including those we have no crabs at
    val crabPositions = (0 to input.max).map(pos => CrabPosition(pos, crabCountByPosition.get(pos).getOrElse(0))).toList
    val totalFuelByPositionForward  = totalFuelByPositionOneWay(crabPositions).toList.sortBy(_._1)
    val totalFuelByPositionBackward = totalFuelByPositionOneWay(crabPositions.reverse).toList.sortBy(_._1)
    val finalFuelByPosition = totalFuelByPositionForward.zip(totalFuelByPositionBackward).map {
      case ((_, forward), (_, backward)) => forward + backward
    }
    finalFuelByPosition.min

  def part1computeTotalFuelByPositionOneWay(crabPositions: List[CrabPosition]): Map[Int, Int] =
    crabPositions.tail
      .foldLeft((Map(crabPositions(0).position -> 0), 0, crabPositions(0), crabPositions(0).numberOfCrabs)) {
        case ((totalFuelByPosition, fuelToThisPoint, previousCrab, crabsSoFar), nextCrab) =>
          val step            = math.abs(nextCrab.position - previousCrab.position)
          val fuelForThisStep = crabsSoFar * step
          val totalFuel       = fuelToThisPoint + fuelForThisStep
          val newTotalFuelByPosition =
            totalFuelByPosition.updatedWith(nextCrab.position)(_.map(_ + totalFuel).orElse(Some(totalFuel)))
          (newTotalFuelByPosition, totalFuel, nextCrab, crabsSoFar + nextCrab.numberOfCrabs)
      }
      ._1

  def part2ComputeTotalFuelByPositionOneWay(crabPositions: List[CrabPosition]): Map[Int, Int] =
    crabPositions.tail
      .foldLeft((Map(crabPositions(0).position -> 0), Map(crabPositions(0) -> 1), 0, crabPositions(0))) {
        case ((totalFuelByPosition, fuelCostByPosition, fuelToThisPoint, previousCrab), nextCrab) =>
          val step            = math.abs(nextCrab.position - previousCrab.position)
          val fuelForThisStep = fuelCostByPosition.map((crab, fuelCost) => crab.numberOfCrabs * fuelCost * step).sum
          val totalFuel       = fuelToThisPoint + fuelForThisStep
          val newTotalFuelByPosition =
            totalFuelByPosition.updatedWith(nextCrab.position)(_.map(_ + totalFuel).orElse(Some(totalFuel)))
          val newFuelCostByPosition = fuelCostByPosition.map((pos, cost) => (pos, cost + 1)).updated(nextCrab, 1)
          (newTotalFuelByPosition, newFuelCostByPosition, totalFuel, nextCrab)
      }
      ._1

  object Parsing:
    def parseInput(input: List[String]): List[Int] =
      input(0).split(",").map(_.toInt).toList
