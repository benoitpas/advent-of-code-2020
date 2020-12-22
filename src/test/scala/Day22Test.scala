import org.junit.Test
import org.junit.Assert._

import Day22._

class Day22Test {
  @Test def testSmallInput1(): Unit = {
    assertEquals(306, score1(smallInput))
  }

  @Test def testFullInput1(): Unit = {
    assertEquals(30197, score1(fullInput))
  }

  @Test def testverySmallInput2(): Unit = {
    assertEquals(78, score2(verySmallInput))
  }


  @Test def testSmallInput2(): Unit = {
    assertEquals(291, score2(smallInput))
  }

  @Test def testFullInput2(): Unit = {
    assertEquals(34031, score2(fullInput))
  }

}