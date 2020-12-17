import org.junit.Test
import org.junit.Assert._

import Day17._

class Day17Test {
  @Test def testPart1a(): Unit = {
    val points = parseInput(smallInput)
    assertEquals(112, iterate(points,3))
  }

  @Test def testPart1b(): Unit = {
    val allPoints = parseInput(fullInput)
    assertEquals(348, iterate(allPoints,3))
  }

  @Test def testPart2a(): Unit = {
    val points = parseInput(smallInput)
    assertEquals(848, iterate(points,4))
  }

  @Test def testPart2b(): Unit = {
    val allPoints = parseInput(fullInput)
    assertEquals(2236, iterate(allPoints,4))
  }

}

  