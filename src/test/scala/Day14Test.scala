import org.junit.Test
import org.junit.Assert._

import Day14._

class Day14Test {
  @Test def testPart1a(): Unit = {
    assertEquals(165, processInput(smallInput))
  }
  
  @Test def testPart1b(): Unit = {
    assertEquals(13496669152158L, processInput(fullInput))
  }
}