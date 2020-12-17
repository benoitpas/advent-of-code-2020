import org.junit.Test
import org.junit.Assert._

import Day17._

class Day17Test {
  @Test def testPart1a(): Unit = {
    val points = parseInput(smallInput)
    assertEquals(112, iterate(points))
  }

  @Test def testPart1b(): Unit = {
    val allPoints = parseInput(fullInput)
    assertEquals(348, iterate(allPoints))
  }

}

  