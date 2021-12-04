package dev.dantb
package adventofcode

import scala.io.Source

object Day2:
  enum Command(val size: Int):
    case Forward(s: Int) extends Command(s)
    case Down(s: Int) extends Command(s)
    case Up(s: Int) extends Command(s)

  def parseCommand(input: String): Command = input.split(" ").nn.toList match
    case "forward" :: size :: Nil => Command.Forward(size.nn.toInt)
    case "down" :: size :: Nil => Command.Down(size.nn.toInt)
    case "up" :: size :: Nil => Command.Up(size.nn.toInt)
    case other => throw new IllegalArgumentException(s"Invalid input $other")

  def readFromFile: List[Command] = Utils.readFromFile("day2-aoc.txt").map(parseCommand)

  object Part1:
    final case class Position(horiz: Int, depth: Int):
      def answer: Int = horiz * depth
      def down(steps: Int): Position = Position(horiz, depth + steps)
      def up(steps: Int): Position = Position(horiz, depth - steps)
      def forward(steps: Int): Position = Position(horiz + steps, depth)

    def solve: Int = compute(readFromFile)

    def compute(input: List[Command]): Int =
      input
        .foldLeft(Position(0, 0)) {
          case (acc, next) =>
            next match
              case Command.Forward(size) => acc.forward(size)
              case Command.Down(size) => acc.down(size)
              case Command.Up(size) => acc.up(size)
        }
        .answer

  object Part2:
    final case class Position(
        horiz: Int,
        depth: Int,
        aim: Int,
      ):
      def answer: Int = horiz * depth
      def down(steps: Int): Position = Position(horiz, depth, aim + steps)
      def up(steps: Int): Position = Position(horiz, depth, aim - steps)
      def forward(steps: Int): Position = Position(horiz + steps, depth + (aim * steps), aim)

    def solve: Int = compute(readFromFile)

    def compute(input: List[Command]): Int =
      input
        .foldLeft(Position(0, 0, 0)) {
          case (acc, next) =>
            next match
              case Command.Forward(size) => acc.forward(size)
              case Command.Down(size) => acc.down(size)
              case Command.Up(size) => acc.up(size)
        }
        .answer
