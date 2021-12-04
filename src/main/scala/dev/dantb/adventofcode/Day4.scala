package dev.dantb
package adventofcode

import scala.io.Source

object Day4:
  case class Position(row: Int, col: Int)
  case class Input(bingoNumbers: List[Int], boards: List[BingoBoard])
  case class BingoBoard(size: Int, numbers: Map[Int, BingoNumber]):
    def mark(num: Int): BingoBoard = BingoBoard(size, numbers.updatedWith(num)(_.map(_.mark)))

  case class BingoNumber(
      num: Int,
      pos: Position,
      marked: Boolean,
    ):
    def mark: BingoNumber = BingoNumber(num, pos, true)

  enum Result:
    case Complete(board: BingoBoard, finalNumber: Int)
    case InProgress(boards: List[BingoBoard])

  def solve: Int =
    val rawInput = Utils.readFromFile("day4-aoc.txt")
    val input = Parsing.parseInput(rawInput)
    Utils.assertDay4InputRoundTrip(rawInput, input)
    playBingo(input)

  def solvePart2: Int = playBingoToLose(Parsing.parseInput(Utils.readFromFile("day4-aoc.txt")))

  def playBingo(input: Input): Int =
    def numbersLoop(nums: List[Int], result: Result): Result = result match
      case Result.InProgress(boards) =>
        nums match
          case Nil => result
          case n :: ns =>
            val boardsWithIndexes = boards.zipWithIndex
            numbersLoop(ns, boardsLoop(boardsWithIndexes, boardsWithIndexes.map(_.swap).toMap, n))
      case _: Result.Complete => result

    def boardsLoop(
        boards: List[(BingoBoard, Int)],
        allBoards: Map[Int, BingoBoard],
        num: Int,
      ): Result = boards match
      case Nil => Result.InProgress(allBoards.values.toList)
      case (b, idx) :: bs =>
        val newBoard = addToBoard(num, b)
        if checkBoard(newBoard) then Result.Complete(newBoard, num)
        else boardsLoop(bs, allBoards.updated(idx, newBoard), num)

    score(numbersLoop(input.bingoNumbers, Result.InProgress(input.boards)))

  def addToBoard(num: Int, board: BingoBoard): BingoBoard =
    def loop(nums: List[BingoNumber]): BingoBoard = nums match
      case Nil => board
      case n :: ns => if n.num == num then board.mark(num) else loop(ns)

    loop(board.numbers.values.toList)

  def checkBoard(board: BingoBoard): Boolean =
    val marked = board.numbers.values.filter(_.marked)
    val markedRows = marked.groupBy(_.pos.row).find(_._2.size == board.size).nonEmpty
    markedRows || marked.groupBy(_.pos.col).find(_._2.size == board.size).nonEmpty

  def score(result: Result): Int = result match
    case complete: Result.Complete =>
      complete
        .board
        .numbers
        .values
        .collect { case b if !b.marked => b.num }
        .sum * complete.finalNumber
    case _ => 0

  def playBingoToLose(input: Input): Int =
    enum BoardResult:
      case Complete(board: BingoBoard, finalNumber: Int)
      case Incomplete(board: BingoBoard)

    def boardsLoop(
        boards: List[BingoBoard],
        num: Int,
        acc: List[BoardResult],
      ): List[BoardResult] = boards match
      case Nil => acc
      case b :: bs =>
        val newBoard = addToBoard(num, b)
        if checkBoard(newBoard) then boardsLoop(bs, num, BoardResult.Complete(newBoard, num) :: acc)
        else boardsLoop(bs, num, BoardResult.Incomplete(newBoard) :: acc)

    val finalBoardToFinish = input
      .bingoNumbers
      .foldLeft((List.empty[Result.Complete], input.boards)) {
        case ((completeBoards, incompleteBoards), num) =>
          val results = boardsLoop(incompleteBoards, num, Nil)
          val complete = results.collect[Result.Complete] {
            case BoardResult.Complete(b, finalNumber) => Result.Complete(b, finalNumber)
          }
          val incomplete = results.collect { case BoardResult.Incomplete(b) => b }
          (complete ++ completeBoards, incomplete)
      }
      ._1
      .head

    score(finalBoardToFinish)

  object Parsing:
    def parseInput(input: List[String]): Input =
      input match
        case nums :: _ :: firstBoardAndBelow =>
          val bingoNumbers = nums.split(',').map(_.toInt).toList
          val size = splitRow(firstBoardAndBelow.head).size
          val boards = firstBoardAndBelow
            .appended("")
            .foldLeft((List.empty[BingoBoard], Map.empty[Int, BingoNumber], 0)) {
              case ((boards, currentBoardMap, rowInBoard), nextLine) =>
                if nextLine == "" then (BingoBoard(size, currentBoardMap) :: boards, Map.empty, 0)
                else
                  val mapForLine: Map[Int, BingoNumber] = parseLine(nextLine, rowInBoard)
                  (boards, currentBoardMap ++ mapForLine, rowInBoard + 1)
            }
            ._1
          Input(bingoNumbers, boards.reverse)
        case _ => Input(Nil, Nil)

    def parseLine(line: String, row: Int): Map[Int, BingoNumber] =
      splitRow(line)
        .zipWithIndex
        .map((num, col) => num.toInt -> BingoNumber(num.toInt, Position(row, col), false))
        .toMap

    def splitRow(row: String): Array[String] = row.trim.replaceAll("  ", " ").split(' ')
