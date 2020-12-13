import org.junit.Test
import org.junit.Assert._

import Day13._

class Day13Test {
  @Test def testPart1a(): Unit = {
    assertEquals(295, findClosests(smallInput))
  }
  
  @Test def testPart1b(): Unit = {
    assertEquals(2238, findClosests(fullInput))
  }
}