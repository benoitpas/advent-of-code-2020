import org.junit.Test
import org.junit.Assert._

import Day18._

class Day18Test {
  @Test def testPart1a(): Unit = {
    val tests = List(
      ("1 + (2 * 3) + (4 * (5 + 6))", 51),
      ("1 + 2 * 3 + 4 * 5 + 6" , 71),
      ("2 * 3 + (4 * 5)",26),
      ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
      ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",  12240),
      ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632))

    for( (expression,expected) <- tests)
      yield assertEquals(expected, eval(expression))
  }

  @Test def testPart1b(): Unit = {
    assertEquals(18213007238947L, fullEval1(fullInput))
  }

  @Test def testPart2a(): Unit = {
  val tests = List(
    ("1 + 2 * 3 + 4 * 5 + 6", 231),
    ("1 + (2 * 3) + (4 * (5 + 6))",51),
    ("2 * 3 + (4 * 5)", 46),
    ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445),
    ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060),
    ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340))

    for( (expression,expected) <- tests)
      yield assertEquals(expected, Evaluator(expression))
  }

  @Test def testPart2b(): Unit = {
    assertEquals(388966573054664L, fullEval2(fullInput))
  }
}
