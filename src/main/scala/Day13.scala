object Day13 {
  import scala.io.Source
  val fullInput = Source.fromResource("day13-input.txt").getLines.toList

  val smallInput = List("939",
    "7,13,x,x,59,x,31,19")

  def readInput(input:List[String]) = (input(0).toInt, input(1).split(","))

  def closests(targetTime:Int, busTime:Int) = 
    val before = (targetTime / busTime) * busTime
    val after = before + busTime
    val wait = if( before == targetTime) 0 else after-targetTime
    (busTime,wait)

  def findClosests(input:List[String]) = {
    val (targetTime, timesStr) = readInput(input)
    val busTimes = timesStr.filter(_ != "x").map(_.toInt).toList
    val closestTimes = busTimes.map(closests(targetTime,_))
    val (busId, wait) = closestTimes.sortBy(_._2).head
    wait * busId
  }

  def main(args: Array[String]): Unit = {
    println("part1=" + findClosests(fullInput))
  }
}