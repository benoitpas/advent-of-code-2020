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
      yield List(j,k)
    r.toSet

  def incDimension(point:Set[List[Int]], newDimension: Int) = {
    val inc = newDimension - point.head.size
    val incList = List.fill(inc)(0)
    point.map( _ ++ incList)
  }

  def neighbors(point:List[Int]) = 
    def loop(point: List[Int]):List[List[Int]] = point match {
      case head::tail => {
        val combinations = loop(tail)
        combinations.flatMap( (c) => List (
          (head - 1)::c,
          head::c,
          (head + 1)::c))
      }
      case Nil => List(List())
    }
    loop(point).toSet.diff(Set(point))

  def next(points: Set[List[Int]]) = {
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

  def iterate(point:Set[List[Int]], dimension: Int) = {
    val initialGrid = incDimension(point, dimension)
    val grids = (1 to 6).foldLeft(List(initialGrid))((acc,_) => {
      next(acc.head)::acc
    })
    grids.map(_.size).head
  }

  def main(args: Array[String]): Unit = {
    val points = parseInput(fullInput)
    println("part1=" + iterate(points, 3))
    println("part2=" + iterate(points, 4))
  }
}