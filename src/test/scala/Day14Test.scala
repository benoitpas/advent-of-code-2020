import org.junit.Test
import org.junit.Assert._

import Day14._

class Day14Test {
  @Test def testPart1a(): Unit = {
    assertEquals(165, processInput1(smallInput1))
  }
  
  @Test def testPart1b(): Unit = {
    assertEquals(13496669152158L, processInput1(fullInput))
  }

  @Test def testPart2a(): Unit = {
    assertEquals(208, processInput2(smallInput2))
  }

  @Test def testPart2b(): Unit = {
    assertEquals(3278997609887L, processInput2(fullInput))
  }
}

  