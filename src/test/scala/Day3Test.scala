import org.junit.Test
import org.junit.Assert._

class Day3Test {
  @Test def testPart1a(): Unit = {
    assertEquals(7, Day3.countTrees(Day3.terrain1,3,1).toInt)
  }

  @Test def testPart1b(): Unit = {
    assertEquals(195, Day3.countTrees(Day3.terrain2,3,1).toInt)
  }

  @Test def testPart2a(): Unit = {
    assertEquals(2, Day3.countTrees(Day3.terrain1,1,1).toInt)
  }

  @Test def testPart2b(): Unit = {
    assertEquals(3, Day3.countTrees(Day3.terrain1,5,1).toInt)
  }

  @Test def testPart2c(): Unit = {
    assertEquals(4, Day3.countTrees(Day3.terrain1,7,1).toInt)
  }

  @Test def testPart2d(): Unit = {
    assertEquals(2, Day3.countTrees(Day3.terrain1,1,2).toInt)
  }

  @Test def testPart2e(): Unit = {
    assertEquals(336, Day3.countTrees(Day3.terrain1, Day3.directions).toInt)
  }

  @Test def testPart2f(): Unit = {
    assertEquals(3772314000L, Day3.countTrees(Day3.terrain2, Day3.directions))
  }


}