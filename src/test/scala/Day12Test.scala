import org.junit.Test
import org.junit.Assert._

import Day12._

class Day12Test {
  @Test def testPart1a(): Unit = {
    assertEquals(25, iterate1(smallInput))
  }
  
  @Test def testPart1b(): Unit = {
    assertEquals(1148, iterate1(fullInput))
  }

  @Test def testPart2a(): Unit = {
    assertEquals(286, iterate2(smallInput))
  }
  
  @Test def testPart2b(): Unit = {
    assertEquals(52203, iterate2(fullInput))
  }

}