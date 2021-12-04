package dev.dantb
package adventofcode

import scala.io.Source

object Utils:
  def readFromFile(fileName: String): List[String] =
    val source = Source.fromResource(fileName)
    val lines = source.getLines.toList
    source.close
    lines
