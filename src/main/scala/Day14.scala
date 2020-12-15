object Day14 {
  import scala.io.Source
  val fullInput = Source.fromResource("day14-input.txt").getLines.toList

  val smallInput1 = List(
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    "mem[8] = 11",
    "mem[7] = 101",
    "mem[8] = 0")

    val smallInput2 = List(
      "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1")

  case class Status1(memory:Map[Int,Long], mask:(Long,Long)) {
    def updateMask(s: String): Status1 = 
      val newMasks = s.foldLeft((0L,0L))((masks,c) => c match {
        case '0' => (masks._1 << 1, masks._2 << 1)
        case '1' => (masks._1 << 1 | 1, masks._2 << 1 | 1)
        case 'X' => (masks._1 << 1, masks._2 << 1 | 1)
      })
      Status1(memory, newMasks)

    def updateMemory(index:Int, value:Long) = {
      val newMemory = memory ++ List[(Int,Long)]((index, (value | mask._1) & mask._2))
      Status1(newMemory, mask)
    }
  }
  object Status1 {
    val iMask = 0x7FFFFFFFFL
    val start = Status1(Map[Int,Long](), (0L,iMask))
  }

  def processInput1(input:List[String]) = {
    val finalMemory = input.foldLeft(Status1.start)((status, line) => line.split(" = ").toList match {
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

  case class Status2(memory:Map[Long,Long], masks:List[(Long,Long)]) {
    def updateMask(s: String): Status2 = 
      val newMasks = s.foldLeft(List[(Long,Long)]((0L,0L)))((masks,c) => c match {
        case '0' => masks.map((m) => (m._1 << 1, m._2 << 1 | 1))
        case '1' => masks.map((m) => (m._1 << 1 | 1, m._2 << 1 | 1))
        case 'X' => masks.flatMap((m) => List(
          (m._1 << 1, m._2 << 1),
          (m._1 << 1 | 1, m._2 << 1 | 1)))
      })
      Status2(memory, newMasks)

    def updateMemory(index:Int, value:Long) = {
      val newAssignments = masks.map((m) => ( ((index.toLong | m._1) & m._2) -> value))
      val newMemory = memory ++ newAssignments
      Status2(newMemory, masks)
    }
  }
  object Status2 {
    val iMask = 0x7FFFFFFFFL
    val start = Status2(Map[Long,Long](), List[(Long,Long)]((0,iMask)))
  }

  def processInput2(input:List[String]) = {
    val finalMemory = input.foldLeft(Status2.start)((status, line) => line.split(" = ").toList match {
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
    println("part1=" + processInput1(fullInput))
    println("part1=" + processInput2(fullInput))
  }
}