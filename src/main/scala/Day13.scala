object Day13 {
  import scala.io.Source
  val fullInput = Source.fromResource("day13-input.txt").getLines.toList

  val smallInput = List("939", "7,13,x,x,59,x,31,19")

  def readInput(input:List[String]) = (input(0).toInt, input(1).split(","))

  // Part 1: find next bus
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

  // Part 2: Contest
  def contestFindTime(input:List[String]) = {
    val (_, timesStr) = readInput(input)

    val firstBus = timesStr.head.toInt
    val busTime = timesStr.tail.foldLeft(List[(Long,Long)]((firstBus,0)))((acc,e) => (e, acc) match {
      case ("x", (-1,c)::tail) => (-1,c+1)::tail
      case ("x", head::tail) => (-1,1 + head._2)::head::tail
      case (v, (-1,c)::tail) => (v.toInt,c+1)::tail
      case (v, head::tail) => (v.toInt, 1 + head._2):: head::tail
    }).reverse

    def mergeBusTimes(inc1:Long, start1:Long, inc2:Long, start2:Long) : (Long,Long) =
      val inc = inc1 * inc2
      val start =
        (for(i <- 0L to inc1 - 1 if ( (start1 + inc2 * i - start2) % inc1 == 0))
          yield (inc + start2 - inc2 *i) % inc)(0)
      (inc, start)

    def mergeBusTimes2(busTime1: (Long,Long), busTime2: (Long,Long)) : (Long,Long) =
      if (busTime1._2 < busTime2._2)
        mergeBusTimes(busTime1._1, busTime1._2, busTime2._1, busTime2._2)
      else
        mergeBusTimes(busTime2._1, busTime2._2, busTime1._1, busTime1._2)

    val r = busTime.reduce(mergeBusTimes2)
    r._1 - r._2
  }


  def main(args: Array[String]): Unit =
    println("part1=" + findClosests(fullInput))
    println("part2=" + contestFindTime(fullInput))
}