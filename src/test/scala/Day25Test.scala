import org.junit.Test
import org.junit.Assert._

import Day25._

class Day25Test {

  @Test def testCardPublicKey1(): Unit = {
    assertEquals(5764801, getPublicKey(8))
  }

  @Test def testCardPublicKey2(): Unit = {
    assertEquals(8, findLoopSize(5764801))
  }

  @Test def testDoorPublicKey1(): Unit = {
    assertEquals(17807724, getPublicKey(11))
  }

  @Test def testDoorPublicKey2(): Unit = {
    assertEquals(11, findLoopSize(17807724))
  }

  @Test def testEncryptionKey1() : Unit = {
    assertEquals(14897079, getKey(17807724,8))
  }

  @Test def testEncryptionKey2() : Unit = {
    assertEquals(14897079, getKey(5764801,11))
  }


  @Test def testFindEncryptionKey() : Unit = {
    assertEquals(Some(14897079), findEncryptionKey(5764801,17807724))
  }
}