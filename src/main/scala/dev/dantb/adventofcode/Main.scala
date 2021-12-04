package dev.dantb
package adventofcode

@main def Main(args: String*): Unit =
  println("─" * 100)
  println("Advent of Code 2021")
  println("─" * 100)

  println(s"Day 1, part 1: ${Day1.solvePart1}")
  println(s"Day 1, part 2: ${Day1.solvePart2}")
  println(s"Day 1, part 2 imperatively: ${Day1.solveAlt}")

  println(s"Day 2, part 1: ${Day2.Part1.solve}")
  println(s"Day 2, part 2: ${Day2.Part2.solve}")

  println(s"Day 3, part 1: ${Day3.solve}")
  println(s"Day 3, part 2 sample: ${Day3.solvePart2Sample}")
  println(s"Day 3, part 2: ${Day3.solvePart2}")

  println(s"Day 4, part 1: ${Day4.solve}")
