import org.junit.Test
import org.junit.Assert._

import Day19._

class Day19Test {
  @Test def testPart1a(): Unit = {
    val s = "0: 8 11"
    assertEquals(None, OrRule.unapply(s))
    assertEquals(Some(0,LazyList(List(8,11))), SequenceRule.unapply(s))
    assertEquals(None, LetterRule.unapply(s))
  }

  @Test def testPart1b(): Unit = {
    val s = "71: 13 | 92"
    assertEquals(Some(71,LazyList(List(13),List(92))), OrRule.unapply(s))
    assertEquals(None, SequenceRule.unapply(s))
    assertEquals(None, LetterRule.unapply(s))
  }

  @Test def testPart1c(): Unit = {
    val s = "92: \"a\""
    assertEquals(None, OrRule.unapply(s))
    assertEquals(None, SequenceRule.unapply(s))
    assertEquals(Some((92,'a')), LetterRule.unapply(s))
  }

  @Test def testPart1d(): Unit = {
    assertEquals(2, countValidMessages1(smallInput1))
  }

  @Test def testPart1e(): Unit = {
    assertEquals(126, countValidMessages1(fullInput))
  }

  @Test def testPart1f(): Unit = {
    assertEquals(3, countValidMessages1(smallInput2))
  }

  @Test def testPart2a(): Unit = {
    assertEquals(12, countValidMessages2(smallInput2))
  }
}
