import org.junit.Test
import org.junit.Assert._

import Day9._

class Day9Test {
  @Test def testPart1a(): Unit = {    
    assertEquals(127L, findException(smallInput,5).toList(0))
  }

  @Test def testPart1b(): Unit = {    
    assertEquals(257342611L, findException(fullInput,25).toList(0))
  }

}