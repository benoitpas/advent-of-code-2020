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

  def maxSeatNumber() = fullInput.map(toSeatNumber).reduce(Math.max)

  def main(args: Array[String]): Unit = {
    println("part1=" + maxSeatNumber())
  }
}