import org.junit.Test
import org.junit.Assert._

import Day10._

class Day10Test {
  @Test def testPart1a(): Unit = {
    assertEquals(220, joltDifferencesMultiplied(smallInput))
  }

  @Test def testPart1b(): Unit = {
    assertEquals(2368, joltDifferencesMultiplied(fullInput))
  }

}