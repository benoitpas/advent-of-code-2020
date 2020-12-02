
import org.junit.Test
import org.junit.Assert._

class Day1Test {
  @Test def test(): Unit = {
    val numbers = List(1721, 979, 366, 299, 675, 1456)
    val pair = Day1.findPair(numbers)(0)
    assertEquals(514579, pair._1 * pair._2)
  }
}