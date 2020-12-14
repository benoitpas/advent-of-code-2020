object Day14 {
  import scala.io.Source
  val fullInput = Source.fromResource("day14-input.txt").getLines.toList

  val smallInput = List(
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    "mem[8] = 11",
    "mem[7] = 101",
    "mem[8] = 0")

  case class Status(memory:Map[Int,Long], mask:(Long,Long)) {
    def updateMask(s: String): Status = 
      val newMasks = s.foldLeft((0L,0L))((masks,c) => c match {
        case '0' => (masks._1 << 1, masks._2 << 1)
        case '1' => (masks._1 << 1 | 1, masks._2 << 1 | 1)
        case 'X' => (masks._1 << 1, masks._2 << 1 | 1)
      })
      Status(memory, newMasks)

    def updateMemory(index:Int, value:Long) = {
      val newMemory = memory ++ List[(Int,Long)]((index, (value | mask._1) & mask._2))
      Status(newMemory, mask)
    }
  }
  object Status {
    val iMask = 0x7FFFFFFFFL
    val start = Status(Map[Int,Long](), (0L,iMask))
  }

  def processInput(input:List[String]) = {
    val finalMemory = input.foldLeft(Status.start)((status, line) => line.split(" = ").toList match {
      case "mask"::m::Nil => status.updateMask(m)
      case left::right::Nil => {
        val leftRegex = "mem\\[(\\d+)\\]".r
        leftRegex.findFirstMatchIn(left) match {
          case Some(index) => status.updateMemory(index.group(1).toInt, right.toLong)
        }
      }
    })
    finalMemory.memory.values.sum
  }
  def main(args: Array[String]): Unit = {
    println("part1=" + processInput(fullInput))
  }
}