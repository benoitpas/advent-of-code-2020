import org.junit.Test
import org.junit.Assert._

import Day21._

class Day21Test {
  @Test def testSmallInput(): Unit = {
    val rules = parse(smallInput)
    val m = matchAllergens(rules)
    assertEquals(5, m._1.length)
    assertEquals("mxmxvkd,sqjhc,fvjkl", allergenIngredients(m._2))
  }

  @Test def testFullInput(): Unit = {
    val rules = parse(fullInput)
    val m = matchAllergens(rules)
    assertEquals(2307, m._1.length)
    assertEquals("cljf,frtfg,vvfjj,qmrps,hvnkk,qnvx,cpxmpc,qsjszn", allergenIngredients(m._2))
  }

}