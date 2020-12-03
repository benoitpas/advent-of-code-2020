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

    def countTrees(terrain : Seq[String]) = {
      val y = 1 to terrain.length-1
      val x = y.map(_*3)
      (x zip y).map((x,y) => {
        val row = terrain(y)
        row(x % row.length)
      }).filter(_ == '#').length
    } 


    def main(args: Array[String]): Unit = {
      println("part1="+countTrees(terrain2))
    }
}