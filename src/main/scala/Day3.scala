object Day3 {

  import scala.io.Source
   
  val terrain2 = Source.fromResource("day3-input.txt").getLines.to(LazyList)
  
  val terrain1 = Array(
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#").toIndexedSeq

    def countTrees(terrain:Seq[String], directions: List[(Int,Int)]) : Long  = 
      directions.map((right,down) => countTrees(terrain, right, down)).reduce( _ * _)

    def countTrees(terrain : Seq[String], right: Int, down: Int) : Long = {
      val y = (down to terrain.length-1 by down) 
      val x = (1 to y.length).map(_ * right)

      (x zip y).map((x,y) => {
        val row = terrain(y)
        row(x % row.length)
      }).filter(_ == '#').length
    } 

    val directions = List((1,1),(3,1),(5,1),(7,1),(1,2))

    def main(args: Array[String]): Unit = {
      println("part1="+countTrees(terrain2,3,1))
      println("part2="+countTrees(terrain2,directions))
    }
}