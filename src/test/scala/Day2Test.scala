
import org.junit.Test
import org.junit.Assert._

class Day2Test {

  val passwords = List("1-3 a: abcde","1-3 b: cdefg","2-9 c: ccccccccc")

  @Test def testCheck1(): Unit = {
    val expected = List(1,0,1)
    val checks = Day2.checkPassword1(passwords)
    assertEquals(expected, checks)
  }

  @Test def testCheck2(): Unit = {
    val expected = List(1,0,0)
    val checks = Day2.checkPassword2(passwords)
    assertEquals(expected, checks)
  }
}