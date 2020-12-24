import org.junit.Test
import org.junit.Assert._

import Day23._

class Day23Test {

  @Test def test10Rounds(): Unit = {
    val exampleList = inputToList(exampleInput)
    val r10 = iterate(exampleList, exampleInput.length, 10)
    assertEquals(List(1, 9, 2, 6, 5, 8, 3, 7, 4), r10.dropRight(1))
    assertEquals("92658374", finalOrder(r10).mkString)
  }

  @Test def test100Rounds(): Unit = {
    assertEquals("67384529", labels(exampleInput))
  }

  @Test def testPart1(): Unit = {
    assertEquals("26354798", labels(puzzleInput))
  }
  @Test def testPart2(): Unit = {
    assertEquals(149245887792L, product(exampleInput))
  }

}