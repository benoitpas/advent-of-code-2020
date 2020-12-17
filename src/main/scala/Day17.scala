object Day17 {
  import scala.io.Source
  val fullInput = Source.fromResource("day17-input.txt").getLines.to(LazyList)

  val smallInput = LazyList(
    ".#.",
    "..#",
    "###")

  def parseInput(input:LazyList[String]) = 
    val r = for (
      (row,j) <- input.zipWithIndex;
      (elem,k) <- row.zipWithIndex
      if (elem == '#'))
      yield (0,j,k)
    r.toSet

  def neighbors(point:(Int,Int,Int)) = 
    val r = for(
      i <- -1 + point._1 to 1 + point._1;
      j <- -1 + point._2 to 1 + point._2;
      k <- -1 + point._3 to 1 + point._3
      if (i != point._1 || j != point._2 || k != point._3))
      yield (i,j,k)
    r.toSet

  def next(points: Set[(Int,Int,Int)]) = {
    (for (point <- points) yield {
      val pointNeighbors = neighbors(point)
      val nbOccupiedNeighbors = pointNeighbors.intersect(points).size

      val r1 = 
        if( nbOccupiedNeighbors == 2 || nbOccupiedNeighbors == 3) {
          Set(point)
        } else Set()

      val inoccupiedNeighbors = pointNeighbors.diff(points)
      val r2 = inoccupiedNeighbors.flatMap((p) => {
        val nbOccupiedNeighbors = neighbors(p).intersect(points).size
        if (nbOccupiedNeighbors == 3) Set(p) else Set()
      })

      r1.union(r2)
    }).flatten
  }

  def iterate(point:Set[(Int,Int,Int)]) = {
    val grids = (1 to 6).foldLeft(List(point))((acc,_) => {
      next(acc.head)::acc
    })
    grids.map(_.size).head
  }

  def main(args: Array[String]): Unit = {
    val allPoints = parseInput(fullInput)
    println("part1="+iterate(allPoints))
  }
}