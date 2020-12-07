import org.junit.Test
import org.junit.Assert._

import Day7._

class Day7Test {
  @Test def testPart1a(): Unit = {    
    assertEquals(4, nbBagContain(bt,rulesMap))
  }

  @Test def testPart1b(): Unit = {    
    assertEquals(112, nbBagContain(bt,allRulesMap))
  }

}