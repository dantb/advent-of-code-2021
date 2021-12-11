package dev.dantb
package adventofcode

import Day10.Bracket.*
import Day10.Direction.*

object Day10:
  def solveSample: Long      = computePart1(Parsing.parseInput(Utils.readFromFile("day10-sample.txt")))
  def solvePart1: Long       = computePart1(Parsing.parseInput(Utils.readFromFile("day10.txt")))
  def solvePart2Sample: Long = computePart2(Parsing.parseInput(Utils.readFromFile("day10-sample.txt")))
  def solvePart2: Long       = computePart2(Parsing.parseInput(Utils.readFromFile("day10.txt")))

  def computePart1(input: List[Line]): Long =
    getCorruptedLines(input, Nil).map((_, char) => pointsForBracket(char.bracket)).sum

  def getCorruptedLines(lines: List[Line], corrupted: List[(Line, Input)]): List[(Line, Input)] = lines match
    case Nil => corrupted
    case l :: ls =>
      firstCorruptedChar(l.inputs, Nil, None) match
        case Some(char) => getCorruptedLines(ls, (l, char) :: corrupted)
        case None       => getCorruptedLines(ls, corrupted)

  def firstCorruptedChar(inputs: List[Input], stack: List[Input], acc: Option[Input]): Option[Input] = inputs match
    case Nil => acc
    case i :: is =>
      if i.direction == Open then firstCorruptedChar(is, i :: stack, acc)
      else
        stack match
          case first :: tail => if Input.matches(first, i) then firstCorruptedChar(is, tail, acc) else Some(i)
          case Nil           => Some(i)

  def pointsForBracket(bracket: Bracket): Long = bracket match
    case Curly  => 1197
    case Square => 57
    case Circle => 3
    case Angle  => 25137

  def computePart2(lines: List[Line]): Long =
    val nonCorrupted = lines.toSet.diff(getCorruptedLines(lines, Nil).map(_._1).toSet)
    val scores = autoCompleteLines(nonCorrupted.toList, Nil).map((_, autoCompleteBrackets) =>
      computeAutoCompleteScore(autoCompleteBrackets, 0)
    )
    scores.sorted.apply((scores.size - 1) / 2)

  def autoCompleteLines(lines: List[Line], incomplete: List[(Line, List[Bracket])]): List[(Line, List[Bracket])] =
    lines match
      case Nil => incomplete
      case l :: ls =>
        autoCompleteLine(l.inputs, Nil, None) match
          case Some(brackets) => autoCompleteLines(ls, (l, brackets) :: incomplete)
          case None           => autoCompleteLines(ls, incomplete)

  def autoCompleteLine(inputs: List[Input], stack: List[Input], acc: Option[List[Bracket]]): Option[List[Bracket]] =
    inputs match
      case Nil =>
        stack
          .foldLeft(acc)((maybeBrackets, next) => maybeBrackets.map(next.bracket :: _).orElse(Some(List(next.bracket))))
          .map(_.reverse)
      case i :: is =>
        if i.direction == Open then autoCompleteLine(is, i :: stack, acc)
        else
          stack match
            case first :: tail => if Input.matches(first, i) then autoCompleteLine(is, tail, acc) else acc
            case Nil           => acc

  def computeAutoCompleteScore(brackets: List[Bracket], acc: Long): Long = brackets match
    case Nil => acc
    case b :: bs =>
      b match
        case Circle => computeAutoCompleteScore(bs, (acc * 5) + 1)
        case Square => computeAutoCompleteScore(bs, (acc * 5) + 2)
        case Curly  => computeAutoCompleteScore(bs, (acc * 5) + 3)
        case Angle  => computeAutoCompleteScore(bs, (acc * 5) + 4)

  def printInput(input: List[Line]): Unit =
    println(s"Input is:\n${input.map(_.inputs.map(Input.toChar).mkString).mkString("\n")}")

  case class Line(inputs: List[Input])
  case class Input(bracket: Bracket, direction: Direction)
  object Input:
    def parse(char: Char): Option[Input] = char match
      case '{' => Some(Input(Curly, Open))
      case '[' => Some(Input(Square, Open))
      case '(' => Some(Input(Circle, Open))
      case '<' => Some(Input(Angle, Open))
      case '}' => Some(Input(Curly, Close))
      case ']' => Some(Input(Square, Close))
      case ')' => Some(Input(Circle, Close))
      case '>' => Some(Input(Angle, Close))
      case _   => None

    def toChar(input: Input): Char = input match
      case Input(Curly, Open)   => '{'
      case Input(Square, Open)  => '['
      case Input(Circle, Open)  => '('
      case Input(Angle, Open)   => '<'
      case Input(Curly, Close)  => '}'
      case Input(Square, Close) => ']'
      case Input(Circle, Close) => ')'
      case Input(Angle, Close)  => '>'

    def matches(open: Input, close: Input): Boolean = open.bracket == close.bracket

  enum Bracket:
    case Curly
    case Square
    case Circle
    case Angle

  enum Direction:
    case Open
    case Close

  object Parsing:
    def parseInput(input: List[String]): List[Line] =
      input.map(line => Line(line.toList.flatMap(Input.parse(_))))
