import org.junit.Test
import org.junit.Assert._

import Day20._

class Day20Test {
  @Test def testPart1a(): Unit = {
    assertEquals(20899048083289L, productCorners(smallInput))
  }

  @Test def testPart1b(): Unit = {
    assertEquals(14986175499719L, productCorners(fullInput))
  }

  @Test def testPart2a(): Unit = {
    val t = Tile(0,
      (0 to 2).map((i) => ('1' to '3').map((j) => (j+i*3).toChar).toVector).toVector)
    assertEquals(Vector(Vector('5')), removeBorders(t))
  }

  @Test def testPart2b(): Unit = {
    assertEquals(273, waterRoughness(smallInput))
  }

  @Test def testPart2c(): Unit = {
    assertEquals(2161, waterRoughness(fullInput))
  }
}