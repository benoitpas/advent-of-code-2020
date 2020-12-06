object Day5 {

  import scala.io.Source
  val fullInput = Source.fromResource("day5-input.txt").getLines.to(LazyList)

  def toSeatNumber(s:String) = {
    val binary = s.map {
      case 'F' => 0
      case 'L' => 0
      case 'B' => 1
      case 'R' => 1
    }.mkString
    Integer.parseInt(binary, 2)
  }

  val allocatedSeats = fullInput.map(toSeatNumber)
  val maxSeatNumber = allocatedSeats.reduce(Math.max)

  val findMissingNumber = {
    val sortedSeatsNumber = allocatedSeats.sorted
    val emptySlots = (sortedSeatsNumber.tail zip sortedSeatsNumber.dropRight(1)).flatMap(pair => {
      val d = pair._1 - pair._2
      if (d > 1) List(pair) else List()
    }).toList
    emptySlots(0)._1 - 1
  }

  def main(args: Array[String]): Unit = {
    println("part1=" + maxSeatNumber)
    println("part2=" + findMissingNumber)
  }
}