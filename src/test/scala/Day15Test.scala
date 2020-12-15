import org.junit.Test
import org.junit.Assert._

import Day15._

class Day15Test {
  @Test def testPart(): Unit = {
    val tests = List(
      (436,  smallList),
      (1,    List(1,3,2)),
      (10,   List(2,1,3)),
      (27,   List(1,2,3)),
      (78,   List(2,3,1)),
      (438,  List(3,2,1)),
      (1836, List(3,1,2)),
      (1428, fullList))

    for ((expected,list) <- tests) 
      yield assertEquals(expected, find2020(list))
  }
  
}

  