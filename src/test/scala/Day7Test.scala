import org.junit.Test
import org.junit.Assert._

import Day7._

class Day7Test {
  @Test def testPart1a(): Unit = {    
    assertEquals(4, nbBagContain(bt,rules1Map))
  }

  @Test def testPart1b(): Unit = {    
    assertEquals(112, nbBagContain(bt,allRulesMap))
  }

  @Test def testPart2a(): Unit = {    
    assertEquals(32, containCount(bt,rules1Map))
  }

  @Test def testPart2b(): Unit = {    
    assertEquals(126, containCount(bt,rules2Map))
  }

  @Test def testPart2c(): Unit = {    
    assertEquals(6260, containCount(bt,allRulesMap))
  }

}