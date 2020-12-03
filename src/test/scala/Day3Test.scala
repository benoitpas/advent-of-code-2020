import org.junit.Test
import org.junit.Assert._

class Day3Test {
  @Test def testPart1a(): Unit = {
    assertEquals(7, Day3.countTrees(Day3.terrain1))
  }

  @Test def testPart1b(): Unit = {
    assertEquals(195, Day3.countTrees(Day3.terrain2))
  }

}