import org.junit.Test
import org.junit.Assert._

import Day11._

class Day11Test {
  @Test def testPart1a(): Unit = {
    assertEquals(37, iterate1(smallInput))
  }
  
  @Test def testPart1b(): Unit = {
    assertEquals(2289, iterate1(fullInput))
  }

  @Test def testPart2a(): Unit = {
    assertEquals(26, iterate2(smallInput))
  }
  
  @Test def testPart2b(): Unit = {
    assertEquals(2059, iterate2(fullInput))
  }
}