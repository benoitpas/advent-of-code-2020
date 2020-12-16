import org.junit.Test
import org.junit.Assert._

import Day16._

class Day16Test {
  @Test def testPart1a(): Unit = {
    val r = parseInput(smallInput)
    assertEquals(71, addInvalidValues(r._2,r._1))
  }

  @Test def testPart1b(): Unit = {
    val r = parseInput(fullInput)
    assertEquals(26988, addInvalidValues(r._2,r._1))
  }
}

  