import org.junit.Test
import org.junit.Assert._

import Day6._

class Day6Test {
  @Test def testPart1a(): Unit = {    
    assertEquals(11, processAllForms(smallInput))
  }

  @Test def testPart1b(): Unit = {    
    assertEquals(6335, processAllForms(fullInput))
  }

}
