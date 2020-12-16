import org.junit.Test
import org.junit.Assert._

import Day16._

class Day16Test {
  @Test def testPart1a(): Unit = {
    val r = parseInput(smallInput1)
    assertEquals(71, addInvalidValues(r._2,r._1))
  }

  val fullInputParsed = parseInput(fullInput)

  @Test def testPart1b(): Unit = {
    assertEquals(26988, addInvalidValues(fullInputParsed._2,fullInputParsed._1))
  }

  @Test def testPart2a(): Unit = {
    val r = parseInput(smallInput2)
    val mapping = mapFields(r._2,r._1)
    assertEquals(Map("row" -> 0, "class" -> 1, "seat" -> 2), mapping)
    assertEquals(List(11,12,13), r._3.toList)
  }

  @Test def testPart2b(): Unit = {
    assertEquals(426362917709L, departureFieldsProduct(fullInputParsed._2, fullInputParsed._1, fullInputParsed._3))
  }

  //

}

  