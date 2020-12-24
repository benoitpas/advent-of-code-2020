object Day24 {
  import scala.io.Source
  val fullInput = Source.fromResource("day24-input.txt").getLines.to(LazyList)

  val smallInput = LazyList(
    "sesenwnenenewseeswwswswwnenewsewsw",
    "neeenesenwnwwswnenewnwwsewnenwseswesw",
    "seswneswswsenwwnwse",
    "nwnwneseeswswnenewneswwnewseswneseene",
    "swweswneswnenwsewnwneneseenw",
    "eesenwseswswnenwswnwnwsewwnwsene",
    "sewnenenenesenwsewnenwwwse",
    "wenwwweseeeweswwwnwwe",
    "wsweesenenewnwwnwsenewsenwwsesesenwne",
    "neeswseenwwswnwswswnw",
    "nenwswwsewswnenenewsenwsenwnesesenew",
    "enewnwewneswsewnwswenweswnenwsenwsw",
    "sweneswneswneneenwnewenewwneswswnese",
    "swwesenesewenwneswnwwneseswwne",
    "enesenwswwswneneswsenwnewswseenwsese",
    "wnwnesenesenenwwnenwsewesewsesesew",
    "nenewswnwewswnenesenwnesewesw",
    "eneswnwswnwsenenwnwnwwseeswneewsenese",
    "neswnwewnwnwseenwseesewsenwsweewe",
    "wseweeenwnesenwwwswnew")

  type Tile = (Int,Int)
  val neighbors = Map[String,Tile](
    "e" ->  ( 1, 0),
    "se" -> ( 1, 1),
    "sw" -> ( 0, 1),
    "w"  -> (-1, 0),
    "nw" -> (-1,-1),
    "ne" -> ( 0,-1)
  )

  object Direction {
    def unapply(s:String) : Option[(Tile, String)] =
      if (s.length>1 && neighbors.contains(s.substring(0,2)))
        Some((neighbors(s.substring(0,2)), s.substring(2,s.length)))
      else if (s.length>0 && neighbors.contains(s.substring(0,1))) 
        Some((neighbors(s.substring(0,1)), s.substring(1,s.length)))
      else None
  }
  
  def next(currentTile:Tile, s:String): (Tile,String) = 
    s match {
      case Direction(direction, remaing) =>{
        val nextTile = (currentTile._1 + direction._1, currentTile._2 + direction._2)
        (nextTile, remaing)
      }
      case _ => (currentTile,s)
    }
  

  @scala.annotation.tailrec
  def findTile(start: Tile, instructions:String) : Tile =
    val n = next(start, instructions)
    if( n._2 == instructions)
      n._1
    else 
      findTile(n._1, n._2)

  def findTile(instructions:String) : Tile = findTile((0,0), instructions)

  def countFlippedTiles(instructions: LazyList[String]) =
    instructions.groupMapReduce(findTile)(_ => 1)(_ + _).values.map(v => v % 2).sum

  def main(args: Array[String]): Unit = {
    println("part1=" + countFlippedTiles(fullInput))
  }
}
