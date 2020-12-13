import org.junit.Test
import org.junit.Assert._

import Day13._

class Day13Test {
  @Test def testPart1a(): Unit = {
    assertEquals(295, findClosests(smallInput))
  }
  
  @Test def testPart1b(): Unit = {
    assertEquals(2238, findClosests(fullInput))
  }

  @Test def testPart2a(): Unit = {
    assertEquals(1068781, contestFindTime(smallInput))
  }

  @Test def testPart2b(): Unit = {
    val l = List("-1","17,x,13,19")
    assertEquals(3417, contestFindTime(l))
  }

  @Test def testPart2c(): Unit = {
    val l = List("-1","67,7,59,61")
    assertEquals(754018, contestFindTime(l))
  }

  @Test def testPart2d(): Unit = {
    val l = List("-1","67,x,7,59,61")
    assertEquals(779210, contestFindTime(l))
  }

  @Test def testPart2e(): Unit = {
    val l = List("-1","67,7,x,59,61")
    assertEquals(1261476, contestFindTime(l))
  }

  @Test def testPart2f(): Unit = {
    val l = List("-1","1789,37,47,1889")
    assertEquals(1202161486, contestFindTime(l))
  }

  @Test def testPart2g(): Unit = {
    assertEquals(560214575859998L, contestFindTime(fullInput))
  }

}