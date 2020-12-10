import org.junit.Test
import org.junit.Assert._

import Day10._

class Day10Test {
  @Test def testPart1a(): Unit = {
    assertEquals(35, joltDifferencesMultiplied(verySmallInput))
  }
  
  @Test def testPart1b(): Unit = {
    assertEquals(220, joltDifferencesMultiplied(smallInput))
  }

  @Test def testPart1c(): Unit = {
    assertEquals(2368, joltDifferencesMultiplied(fullInput))
  }

  @Test def testPart2a(): Unit = {
    assertEquals(8, countCombinations(verySmallInput))
  }
  
  @Test def testPart2b(): Unit = {
    assertEquals(19208, countCombinations(smallInput))
  }

  @Test def testPart2c(): Unit = {
    assertEquals(1727094849536L, countCombinations(fullInput))
  }


}