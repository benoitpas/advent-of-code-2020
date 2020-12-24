import org.junit.Test
import org.junit.Assert._

import Day24._

class Day24Test {

  @Test def test1a(): Unit = {
    assertEquals((3,0), findTile("esenee"))
  }

  @Test def test1b(): Unit = {
    assertEquals((0,0), findTile("nwwswee"))
  }

  @Test def test1c(): Unit = {
    assertEquals(10, countFlippedTiles(smallInput))
  }

  @Test def test1d(): Unit = {
    assertEquals(322, countFlippedTiles(fullInput))
  }
}