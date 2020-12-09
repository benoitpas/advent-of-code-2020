import org.junit.Test
import org.junit.Assert._

import Day9._

class Day9Test {
  @Test def testPart1a(): Unit = {
    assertEquals(127L, findException(smallInput,5))
  }

  @Test def testPart1b(): Unit = {
    assertEquals(257342611L, findException(fullInput,25))
  }

  @Test def testPart2a(): Unit = {
    assertEquals(62L, ComputeEncrytionWeakness(smallInput,5))
  }

  @Test def testPart2b(): Unit = {
    assertEquals(35602097L, ComputeEncrytionWeakness(fullInput,25))
  }

}