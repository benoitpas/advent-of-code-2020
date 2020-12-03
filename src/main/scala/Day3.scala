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

    def countTrees(terrain : Seq[String], right: Int, down: Int) = {
      val p = (1 to terrain.length-1) map ( i => (i * right, i * down))
      p.map((x,y) => {
        val row = terrain(y)
        row(x % row.length)
      }).filter(_ == '#').length
    } 


    def main(args: Array[String]): Unit = {
      println("part1="+countTrees(terrain2,3,1))
    }
}