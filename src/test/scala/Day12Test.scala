import org.junit.Test
import org.junit.Assert._

import Day12._

class Day12Test {
  @Test def testPart1a(): Unit = {
    assertEquals(25, iterate(smallInput))
  }
  
  @Test def testPart1b(): Unit = {
    assertEquals(1148, iterate(fullInput))
  }
}