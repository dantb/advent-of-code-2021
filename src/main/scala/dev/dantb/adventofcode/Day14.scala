package dev.dantb
package adventofcode

object Day14:
  def solveSample: Long      = part1(Parsing.parseInput(Utils.readFromFile("day14-sample.txt")))
  def solvePart1: Long       = part1(Parsing.parseInput(Utils.readFromFile("day14.txt")))
  def solvePart2Sample: Long = part2(Parsing.parseInput(Utils.readFromFile("day14-sample.txt")))
  def solvePart2: Long       = part2(Parsing.parseInput(Utils.readFromFile("day14.txt")))

  def part1(input: Input): Long = compute(input, 10)
  def part2(input: Input): Long = compute(input, 40)

  def compute(input: Input, steps: Int): Long =
    def buildMap(list: List[Char], acc: Map[(Char, Char), Long]): Map[(Char, Char), Long] = list match
      case first :: second :: rest =>
        buildMap(second :: rest, acc.updatedWith((first, second))(_.map(_ + 1).orElse(Some(1L))))
      case _ => acc

    def computeCounts(
        steps: Int,
        pairCounts: Map[(Char, Char), Long],
        counts: Map[Char, Long]
    ): Map[Char, Long] =
      if steps <= 0 then counts
      else
        val (newPairCounts, newCounts) =
          pairCounts.foldLeft((Map.empty[(Char, Char), Long], counts)) { case ((pairCs, cs), ((fst, snd), count)) =>
            input.rules.get((fst, snd)) match
              case Some(value) =>
                val newPairCs = pairCs
                  .updatedWith((fst, value))(_.map(_ + count).orElse(Some(count)))
                  .updatedWith((value, snd))(_.map(_ + count).orElse(Some(count)))
                val newCs = cs.updatedWith(value)(_.map(_ + count).orElse(Some(count)))
                (newPairCs, newCs)
              case None => (pairCs, cs)
          }
        computeCounts(
          steps - 1,
          newPairCounts,
          newCounts
        )

    val initialPairCounts = buildMap(input.template.toList, Map.empty)
    val initialCounts     = input.template.toList.groupBy(identity).view.mapValues(_.size.toLong).toMap
    val result            = computeCounts(steps, initialPairCounts, initialCounts)
    result.values.max - result.values.min

  case class Input(template: String, rules: Map[(Char, Char), Char])
  object Parsing:
    def parseInput(input: List[String]): Input = input match
      case template :: _ :: others =>
        val rules = others.map { line =>
          val (key :: value :: Nil) = line.replace(" -> ", ",").split(',').toList: @unchecked
          (key(0), key(1)) -> value(0)
        }.toMap
        Input(template, rules)
      case _ => throw new IllegalArgumentException(s"Input format illegal: $input")
