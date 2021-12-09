package dev.dantb
package adventofcode

object Day8:
  def solveSample: Long      = computePart1(Parsing.parseInput(Utils.readFromFile("day8-sample.txt")))
  def solvePart1: Long       = computePart1(Parsing.parseInput(Utils.readFromFile("day8.txt")))
  def solvePart2Sample: Long = computePart2(Parsing.parseInput(Utils.readFromFile("day8-sample.txt")))
  def solvePart2: Long       = computePart2(Parsing.parseInput(Utils.readFromFile("day8.txt")))

  def computePart1(input: List[Entry]): Int =
    input.foldLeft(0) { (sum, next) =>
      next.outputs.map { output =>
        output.size match
          case 2 | 3 | 4 | 7 => 1
          case _             => 0
      }.sum + sum
    }

  final case class UniqueSignals(
      one: Option[String],
      four: Option[String],
      seven: Option[String],
      eight: Option[String]
  ):
    def found: Boolean = one.nonEmpty && four.nonEmpty && seven.nonEmpty && eight.nonEmpty
    def unsafeGetToSet: (Set[Char], Set[Char], Set[Char], Set[Char]) =
      (one.get.toSet, four.get.toSet, seven.get.toSet, eight.get.toSet)
  final case class Entry(patterns: List[String], outputs: List[String]):
    def allSignals: List[String] = patterns ++ outputs

  def computePart2(input: List[Entry]): Int = input.map(computeForEntry).sum

  def computeForEntry(entry: Entry): Int =
    def uniqueSignals(list: List[String], signals: UniqueSignals): UniqueSignals =
      if signals.found then signals
      else
        list match
          case Nil => signals
          case e :: es =>
            uniqueSignals(
              es,
              e.foldLeft(signals) { (currentSignals, output) =>
                e.size match
                  case 2 => currentSignals.copy(one = Some(e))
                  case 4 => currentSignals.copy(four = Some(e))
                  case 3 => currentSignals.copy(seven = Some(e))
                  case 7 => currentSignals.copy(eight = Some(e))
                  case _ => currentSignals
              }
            )

    val signals = uniqueSignals(entry.patterns ++ entry.outputs, UniqueSignals(None, None, None, None))

    // Get signals as sets of chars and start doing set arithmetic
    val (one, four, seven, eight) = signals.unsafeGetToSet

    // To get 9, let X = 4 union 7. Filter to only size 6. The diff between X and 9 should have size 1, the others have size 2.
    val entriesWithSizeSix = entry.allSignals.collect { case signal if signal.size == 6 => signal.toSet }.toSet
    val nine               = entriesWithSizeSix.find(entry => entry.diff(four.union(seven)).size == 1).head

    // To get 2. Diff 9 against each signal with size 5. Only one of these diffs should have size 2, for number 2.
    val entriesWithSizeFive = entry.allSignals.collect { case signal if signal.size == 5 => signal.toSet }.toSet
    val two                 = entriesWithSizeFive.find(entry => nine.diff(entry).size == 2).head

    // To get 3 & 5. 3 has two signals in common with 2, five only has one.
    val entriesSizeFiveWithoutTwo = entriesWithSizeFive.filterNot(_ == two)
    val (threeSet, fiveSet)       = entriesSizeFiveWithoutTwo.partition(entry => two.diff(entry).size == 1)
    val (three, five)             = (threeSet.head, fiveSet.head)

    // To get 0 and 6. 1 union 0 should have size 6, but 1 union 6 has size 7.
    val entriesSizeSixWithoutNine = entriesWithSizeSix.filterNot(_ == nine)
    val (zeroSet, sixSet)         = entriesSizeSixWithoutNine.partition(entry => one.union(entry).size == 6)
    val (zero, six)               = (zeroSet.head, sixSet.head)

    val numsWithIndexes = List(zero, one, two, three, four, five, six, seven, eight, nine).zipWithIndex

    entry.outputs.map(output => numsWithIndexes.find((num, idx) => num == output.toSet).head._2).mkString.toInt

  object Parsing:
    def parseInput(input: List[String]): List[Entry] =
      input.map { line =>
        val (patterns :: outputs :: Nil) = line.split('|').toList: @unchecked
        Entry(patterns.trim.split(' ').toList, outputs.trim.split(' ').toList)
      }
