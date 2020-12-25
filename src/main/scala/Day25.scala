object Day25 {
  import scala.io.Source
  val puzzleInput = Source.fromResource("day25-input.txt").getLines.to(LazyList)
  val puzzleCardPublicKey = puzzleInput.head.toInt
  val puzzleDoorPublicKey = puzzleInput.tail.head.toInt


  val subjectNumber = 7
  def next(v: Long, subjectNumber:Int) : Long = v * subjectNumber % 20201227L

  def getKey(subjectNumber: Int, loopSize: Int) = 
    @scala.annotation.tailrec
    def loop(currentValue:Long, loopSize: Int) : Long = loopSize match {
      case 0 => currentValue
      case i => loop(next(currentValue, subjectNumber), i - 1)
    }
    loop(1, loopSize)

  def getPublicKey(loopSize: Int) = getKey(subjectNumber, loopSize)

  def findLoopSize(publicKey:Int) = 
    @scala.annotation.tailrec
    def loop(currentValue:Long, loopSize: Int) : Int = 
      if (currentValue == publicKey.toLong)
        loopSize
      else
        loop(next(currentValue, subjectNumber), loopSize + 1)
    loop(1,0)

  def findEncryptionKey(cardPublicKey:Int, doorPublicKey:Int) =
    val cardLoopSize = findLoopSize(cardPublicKey)
    val doorLoopSize = findLoopSize(doorPublicKey)
    val cardEncryptionKey = getKey(doorPublicKey, cardLoopSize)
    val doorEncryptionKey = getKey(cardPublicKey, doorLoopSize)
    if (doorEncryptionKey == cardEncryptionKey)
      Some(cardEncryptionKey)
    else
      None

  def main(args: Array[String]): Unit = {
    println("part1=" + findEncryptionKey(puzzleDoorPublicKey,puzzleCardPublicKey).get)
  }
}
