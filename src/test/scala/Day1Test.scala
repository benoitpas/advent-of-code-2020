
import org.junit.Test
import org.junit.Assert._

class Day1Test {
  val numbers = List(1721, 979, 366, 299, 675, 1456)

  @Test def testPair(): Unit = {
    val pair = Day1.findPair(numbers)(0)
    assertEquals(514579, pair._1 * pair._2)
  }

  @Test def testTriple(): Unit = {
    val triple = Day1.findTriple(numbers)(0)
    assertEquals(241861950, triple._1 * triple._2 * triple._3)
  }
}