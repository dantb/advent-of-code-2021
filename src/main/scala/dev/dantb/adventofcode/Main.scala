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
  println(s"Day 4, part 2: ${Day4.solvePart2}")

  println(s"Day 5, part 1: ${Day5.solve}")
  println(s"Day 5, part 2 sample: ${Day5.solveSample}")
  println(s"Day 5, part 2: ${Day5.solvePart2}")

  println(s"Day 6, part 1 sample: ${Day6.solveSample}")
  println(s"Day 6, part 1: ${Day6.solve}")
  println(s"Day 6, part 2: ${Day6.solvePart2}")

  println(s"Day 7, part 1 sample: ${Day7.solveSample}")
  println(s"Day 7, part 1: ${Day7.solve}")
  println(s"Day 7, part 2 sample: ${Day7.solvePart2Sample}")
  println(s"Day 7, part 2: ${Day7.solvePart2}")

  println(s"Day 8, part 1 sample: ${Day8.solveSample}")
  println(s"Day 8, part 1: ${Day8.solvePart1}")
  println(s"Day 8, part 2 sample: ${Day8.solvePart2Sample}")
  println(s"Day 8, part 2: ${Day8.solvePart2}")
