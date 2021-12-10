package dev.dantb
package adventofcode

object Day9:
  def solveSample: Long      = computePart1(Parsing.parseInput(Utils.readFromFile("day9-sample.txt")))
  def solvePart1: Long       = computePart1(Parsing.parseInput(Utils.readFromFile("day9.txt")))
  def solveSamplePart2: Long = computePart2(Parsing.parseInput(Utils.readFromFile("day9-sample.txt")))
  def solvePart2: Long       = computePart2(Parsing.parseInput(Utils.readFromFile("day9.txt")))

  def computePart1(input: Array[Row]): Int =
    lowPointsForGrid(input).map(_.value + 1).sum

  def lowPointsForGrid(input: Array[Row]): List[Candidate] =
    val firstRowCandidates = candidatesForRow(input(0))
    val maxRowIndex        = input.size - 1
    input.zipWithIndex.tail
      .foldLeft((List.empty[Candidate], input(0), firstRowCandidates)) {
        case ((candidates, previousRow, previousRowCandidates), (nextRow, nextRowIndex)) =>
          val filteredPreviousRowCandidates =
            previousRowCandidates.collect {
              case cand if nextRow.numbers(cand.col) > cand.value => cand.toCandidate(nextRowIndex - 1)
            }
          val nextRowCandidates =
            candidatesForRow(nextRow).filter(cand => previousRow.numbers(cand.col) > cand.value)
          val newCandidates: List[Candidate] =
            if nextRowIndex == maxRowIndex then
              nextRowCandidates.map(_.toCandidate(nextRowIndex)) ++ filteredPreviousRowCandidates ++ candidates
            else filteredPreviousRowCandidates ++ candidates
          (newCandidates, nextRow, nextRowCandidates)
      }
      ._1

  def candidatesForRow(row: Row): List[RowCandidate] =
    val maxColIndex = row.numbers.size - 1
    row.numbers.zipWithIndex.tail
      .foldLeft((List.empty[RowCandidate], row.numbers(0), 0, true)) {
        case ((candidates, previous, previousIndex, previousIsCandidate), (next, nextIndex)) =>
          if next < previous then
            val newCandidates =
              if nextIndex == maxColIndex then RowCandidate(nextIndex, next) :: candidates else candidates
            (newCandidates, next, nextIndex, true)
          else if next > previous then
            val newCandidates =
              if previousIsCandidate then RowCandidate(previousIndex, previous) :: candidates else candidates
            (newCandidates, next, nextIndex, false)
          else (candidates, next, nextIndex, false)
      }
      ._1

  def computePart2(input: Array[Row]): Int =
    val totalRows = input.size
    val totalCols = input.head.numbers.size

    def adjacent(pos: Position): Set[Position] = pos match
      case Position(row, col) =>
        val outOfBounds: Position => Boolean =
          case Position(r, c) => r < 0 || r > totalRows - 1 || c < 0 || c > totalCols - 1
        Set(Position(row, col - 1), Position(row, col + 1), Position(row - 1, col), Position(row + 1, col))
          .filterNot(outOfBounds)

    def findPositionsInBasin(positions: Set[Position], visited: Set[Position]): Set[Position] =
      if positions.isEmpty then visited
      else
        val shouldWeVisit: Position => Boolean = p => !visited.contains(p) && input(p.row).numbers(p.col) != 9
        findPositionsInBasin(
          positions.flatMap(adjacent).filter(shouldWeVisit),
          positions ++ visited
        )

    lowPointsForGrid(input)
      .map(lowPoint => findPositionsInBasin(Set(Position(lowPoint.row, lowPoint.col)), Set()).size)
      .sortBy(-_)
      .take(3)
      .product

  case class Position(row: Int, col: Int)
  case class Candidate(row: Int, col: Int, value: Int)
  case class RowCandidate(col: Int, value: Int)
  extension (rowCand: RowCandidate) def toCandidate(row: Int) = Candidate(row, rowCand.col, rowCand.value)
  case class Row(numbers: Array[Int])

  object Parsing:
    def parseInput(input: List[String]): Array[Row] =
      input.map(line => Row(line.map(_.toString.toInt).toArray)).toArray
