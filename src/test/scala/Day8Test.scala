import org.junit.Test
import org.junit.Assert._

import Day8._

class Day8Test {
  @Test def testPart1a(): Unit = {    
    assertEquals(5, run(instructions1)._2)
  }

  @Test def testPart1b(): Unit = {    
    assertEquals(2058, run(allInstructions)._2)
  }

  @Test def testPart2a(): Unit = {    
    assertEquals(8, patch(instructions1)(0)._2)
  }

  @Test def testPart2b(): Unit = {    
    assertEquals(1000, patch(allInstructions)(0)._2)
  }
}