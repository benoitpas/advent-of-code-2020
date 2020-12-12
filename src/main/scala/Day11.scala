object Day11 {
  import scala.io.Source
  val fullInput = Source.fromResource("day11-input.txt").getLines.toList

  val smallInput = List("L.LL.LL.LL",
  "LLLLLLL.LL",
  "L.L.L..L..",
  "LLLL.LL.LL",
  "L.LL.LL.LL",
  "L.LLLLL.LL",
  "..L.L.....",
  "LLLLLLLLLL",
  "L.LLLLLL.L",
  "L.LLLLL.LL")

  type Grid = IndexedSeq[IndexedSeq[Char]]
  def countAdjacentOccupiedSeats(x:Int, y: Int, sitting:Grid) =
    (for(ix <- -1 to 1; iy <- -1 to 1 if (!(ix == 0 && iy == 0))) yield {
      val nx = x + ix
      val ny = y + iy
      if ( 0 <= nx && nx < sitting(y).length &&
        0 <= ny && ny < sitting.length &&
        sitting(ny)(nx) == '#') 1 else 0
    }).reduce(_ + _)
     
  def countVisibleOccupiedSeats(startX:Int, startY: Int, sitting:Grid) =
    (for(ix <- -1 to 1; iy <- -1 to 1 if (!(ix == 0 && iy == 0))) yield {
      import scala.annotation.tailrec
      @tailrec
      def occupied(x:Int, y:Int): Int = {
        val nx = x + ix
        val ny = y + iy
        if( ny < 0 || ny >=  sitting.length 
          || nx < 0 || nx >= sitting(ny).length
          || sitting(ny)(nx) == 'L') 0 else 
            if( sitting(ny)(nx) == '#') 1 else occupied(nx, ny)
      }
      occupied(startX,startY) 
    }).reduce(_ + _)

  def nextState(sitting:Grid, count: (Int,Int,Grid) => Int, threshold: Int) = 
    for (
      y <- (0 to sitting.length - 1)
    ) yield {
      for(x <- (0 to sitting(y).length - 1)) yield {
        sitting(y)(x) match {
          case 'L' if (count(x,y,sitting) == 0) => '#'
          case '#' if (count(x,y,sitting) >= threshold) => 'L'
          case s => s
        }
      }
    }

  def countOccupiedSeats(sitting:Grid) = sitting.flatten.count(_ == '#')

  def str(sitting:Grid) = sitting.map(_.mkString).mkString("\n")

  def simulate(sitting:Grid, count: (Int,Int,Grid) => Int, threshold: Int) : LazyList[(Int,Grid)]= {
    val next = nextState(sitting, count, threshold)
    (countOccupiedSeats(next),next) #:: simulate(next, count, threshold)
  }

  def simulate2(sitting:Grid) : LazyList[(Int,Grid)]= {
    val next = nextState(sitting, countVisibleOccupiedSeats, 5)
    (countOccupiedSeats(next),next) #:: simulate2(next)
  }

  def iterate(input:List[String], count: (Int,Int,Grid) => Int, threshold: Int) = {
    val v = input.map(_.toVector).toVector
    val onlyCounts = simulate(v,count, threshold).map(_._1)
    val compare = onlyCounts zip (-1 #:: onlyCounts)
    compare.filter((p)=> p._1 == p._2).take(1).toList(0)._1
  }

  def iterate1(input:List[String]) = iterate(input,countAdjacentOccupiedSeats,4)

  def iterate2(input:List[String]) = iterate(input,countVisibleOccupiedSeats,5)

  def main(args: Array[String]): Unit = {
    println("part1="+iterate1(fullInput))
    println("part2="+iterate2(fullInput))
  }
}