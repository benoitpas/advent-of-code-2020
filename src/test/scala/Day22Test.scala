import org.junit.Test
import org.junit.Assert._

import Day22._

class Day22Test {
  @Test def testSmallInput(): Unit = {
    assertEquals(306, score(smallInput))
  }

  @Test def testFullInput(): Unit = {
    assertEquals(30197, score(fullInput))
  }
}