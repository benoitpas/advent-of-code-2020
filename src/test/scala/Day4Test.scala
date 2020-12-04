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

  /* Field rules */
  val fieldRuleTestsTxt = """
  byr valid:   2002
  byr invalid: 2003
  
  hgt valid:   60in
  hgt valid:   190cm
  hgt invalid: 190in
  hgt invalid: 190
  
  hcl valid:   #123abc
  hcl invalid: #123abz
  hcl invalid: 123abc
  
  ecl valid:   brn
  ecl invalid: wat
  
  pid valid:   000000001
  pid invalid: 0123456789"""

  val mapStatus = Map("valid:" -> true, "invalid:" -> false)

  val fieldRuleTests = fieldRuleTestsTxt.split("\n").toList.map(_.split(" ")
    .map(_.trim).toList.filter(_.length> 0 ))

  @Test def testFieldRules(): Unit = {

    fieldRuleTests.foreach( (test) => {
      val r =  test match {
        case Nil => true
        case field::status::value::List() => (checkField(field,value) == mapStatus(status))
        case _ => false
      }
      assertTrue(test.toString, r)
    })
  }

  @Test def testPart2(): Unit = {
    
    assertEquals(184, countPassport(passports2, checkPassport2))
  }
}
