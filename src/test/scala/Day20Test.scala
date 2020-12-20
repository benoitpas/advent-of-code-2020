import org.junit.Test
import org.junit.Assert._

import Day20._

class Day20Test {
  @Test def testPart1a(): Unit = {
    assertEquals(20899048083289L, productCorners(smallInput))
  }

  @Test def testPart1b(): Unit = {
    assertEquals(14986175499719L, productCorners(fullInput))
  }

}