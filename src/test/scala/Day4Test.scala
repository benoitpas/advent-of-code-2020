import org.junit.Test
import org.junit.Assert._

import Day4._

class Day4Test {
  @Test def testPart1a(): Unit = {
    assertEquals(2, countPassport(passports1, checkPassport1))
  }

  @Test def testPart1b(): Unit = {
    assertEquals(254, countPassport(passports2, checkPassport1))
  }

}
