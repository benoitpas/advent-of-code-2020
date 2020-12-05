import org.junit.Test
import org.junit.Assert._

import Day5._

class Day5Test {
  @Test def testPart1a(): Unit = {    
    assertEquals(567, toSeatNumber("BFFFBBFRRR"))
  }

  @Test def testPart1b(): Unit = {    
    assertEquals(119, toSeatNumber("FFFBBBFRRR"))
  }

  @Test def testPart1c(): Unit = {    
    assertEquals(820, toSeatNumber("BBFFBBFRLL"))
  }

  @Test def testPart1d(): Unit = {    
    assertEquals(828, maxSeatNumber)
  }

  @Test def testPart2(): Unit = {    
    assertEquals(565, findMissingNumber)
  }

}
