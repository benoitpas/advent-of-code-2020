import org.junit.Test
import org.junit.Assert._

import Day8._

class Day8Test {
  @Test def testPart1a(): Unit = {    
    assertEquals(5, run(instructions1))
  }

  @Test def testPart1b(): Unit = {    
    assertEquals(2058, run(allInstructions1))
  }

}