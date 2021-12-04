package dev.dantb
package adventofcode

import scala.io.Source
import dev.dantb.adventofcode.Day4.*

object Utils:
  def readFromFile(fileName: String): List[String] =
    val source = Source.fromResource(fileName)
    val lines = source.getLines.toList
    source.close
    lines

  // Paranoid
  def assertDay4InputRoundTrip(rawInput: List[String], input: Input): Unit =
    val bingoNumbers = input.bingoNumbers.map(_.toString).mkString(",")

    def writeBoard(board: BingoBoard): List[String] =
      val rows = board
        .numbers
        .values
        .groupBy(_.pos.row)
        .map(
          _._2
            .toList
            .sortBy(_.pos.col)
            .map(x => if x.num.toString.size == 1 then s" ${x.num}" else s"${x.num}")
            .mkString(" ")
        )
        .toList
      "" :: rows

    val roundTrip = bingoNumbers :: input
      .boards
      .foldLeft(List.empty[String])((acc, next) => acc ++ writeBoard(next))

    assert(
      roundTrip == rawInput,
      s"Parsed input round trip failed.\nExpected:\n$rawInput\nActual:$roundTrip",
    )
