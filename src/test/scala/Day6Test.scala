import org.junit.Test
import org.junit.Assert._

import Day6._

class Day6Test {
  @Test def testPart1a(): Unit = {    
    assertEquals(11, processAllForms1(smallInput))
  }

  @Test def testPart1b(): Unit = {    
    assertEquals(6335, processAllForms1(fullInput))
  }

  @Test def testPart2a(): Unit = {    
    assertEquals(6, processAllForms2(smallInput))
  }

  @Test def testPart2b(): Unit = {    
    assertEquals(3392, processAllForms2(fullInput))
  }

}
