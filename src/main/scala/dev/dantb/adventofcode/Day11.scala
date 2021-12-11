package dev.dantb
package adventofcode

object Day11:
  def solveSample: Long       = part1(Parsing.parseInput(Utils.readFromFile("day11-sample.txt")))
  def solvePart1: Long        = part1(Parsing.parseInput(Utils.readFromFile("day11.txt")))
  def solvePart2: Option[Int] = part2(Parsing.parseInput(Utils.readFromFile("day11.txt")))

  def part1(input: Array[Array[Int]]): Int         = compute(input, 100)._1
  def part2(input: Array[Array[Int]]): Option[Int] = compute(input, 1000)._2

  def compute(input: Array[Array[Int]], totalSteps: Int): (Int, Option[Int]) =
    def makeStep(
        step: Int,
        state: Array[Array[Int]],
        flashCount: Int,
        allFlashedAtStep: Option[Int]
    ): (Int, Option[Int]) =
      if step <= 0 then (flashCount, allFlashedAtStep)
      else
        val incremented            = increment(state)
        val initialFlashes         = computeInitialFlashes(incremented)
        val (allFlashes, newState) = findCascadingFlashes(initialFlashes, Set(), incremented)
        val allFlashedThisStep =
          if allFlashes.size == 100 then allFlashedAtStep.orElse(Some(step)) else allFlashedAtStep
        val resetFlashedCells = allFlashes.foldLeft(newState)(updateState(_, _, 0))
        makeStep(step - 1, resetFlashedCells, flashCount + allFlashes.size, allFlashedThisStep)

    val (totalFlashes, allFlashedAtStep) = makeStep(totalSteps, input, 0, None)
    (totalFlashes, allFlashedAtStep.map(totalSteps - _ + 1))

  def computeInitialFlashes(state: Array[Array[Int]]): Set[Position] =
    state.zipWithIndex
      .map { (row, rowIdx) =>
        row.zipWithIndex.collect { case (value, colIdx) if value == 10 => Position(rowIdx, colIdx) }.toSet
      }
      .toSet
      .flatten

  def findCascadingFlashes(
      flashes: Set[Position],
      allFlashes: Set[Position],
      state: Array[Array[Int]]
  ): (Set[Position], Array[Array[Int]]) =
    if flashes.diff(allFlashes).isEmpty then (allFlashes, state)
    else
      val (flashesAmongAdjacents, newState) =
        flashes.map(adjacent(_, state.size, state.head.size)).foldLeft((Set.empty[Position], state)) {
          case ((allNewFlashes, oldState), adjacentPositions) =>
            val (flashesAmongAdjacents, newState) =
              adjacentPositions.foldLeft((Set.empty[Position], oldState)) { case ((newFlashes, oldState0), adjPos) =>
                val value          = oldState0(adjPos.row)(adjPos.col)
                val updatedFlashes = if value == 9 then newFlashes + adjPos else newFlashes
                (updatedFlashes, updateState(oldState0, adjPos, value + 1))
              }
            (allNewFlashes ++ flashesAmongAdjacents, newState)
        }
      findCascadingFlashes(flashesAmongAdjacents, allFlashes ++ flashes, newState)

  def updateState(state: Array[Array[Int]], pos: Position, value: Int): Array[Array[Int]] =
    state.updated(pos.row, state(pos.row).updated(pos.col, value))

  def increment(state: Array[Array[Int]]): Array[Array[Int]] = state.map(_.map(_ + 1))

  def adjacent(pos: Position, totalRows: Int, totalCols: Int): Set[Position] = pos match
    case Position(row, col) =>
      val outOfBounds: Position => Boolean =
        case Position(r, c) => r < 0 || r > totalRows - 1 || c < 0 || c > totalCols - 1
      Set(
        Position(row, col - 1),
        Position(row, col + 1),
        Position(row + 1, col - 1),
        Position(row + 1, col + 1),
        Position(row - 1, col - 1),
        Position(row - 1, col + 1),
        Position(row - 1, col),
        Position(row + 1, col)
      ).filterNot(outOfBounds)

  def printGrid(grid: Array[Array[Int]]): Unit =
    println("\n" ++ grid.map(_.map(_.toString).mkString(",")).mkString("\n") ++ "\n")

  case class Position(row: Int, col: Int)
  object Parsing:
    def parseInput(input: List[String]): Array[Array[Int]] =
      input.map(line => line.map(_.toString.toInt).toArray).toArray
