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

  def getTiles(instructions : LazyList[String]) = instructions.groupMapReduce(findTile)(_ => 1)(_ + _)

  // 1 is black, 0 white
  def getTilesStatus(instructions : LazyList[String]) = getTiles(instructions).map((k,v) => (k, v % 2))

  def countFlippedTiles(instructions: LazyList[String]) =
    getTiles(instructions).values.map(v => v % 2).sum

  def countBlackNeighbors(tile :Tile, status: Map[(Int,Int),Int]) : Int =
    neighbors.map(neighbor => {
      val nTile = (tile._1 + neighbor._2._1, tile._2 + neighbor._2._2)
      status.getOrElse(nTile, 0)
    }).sum

  def addWhiteNeighbors(tiles:Map[(Int,Int),Int]) =
    tiles.foldLeft(tiles)((accTiles,tile) => {
      val newNeighbors = neighbors.flatMap( (n) => {
        val nTile = (tile._1._1 + n._2._1, tile._1._2 + n._2._2)
        accTiles.get(nTile) match {
          case Some(_) => List()
          case None => List(nTile -> 0)
        }
      })
      accTiles ++ newNeighbors
    })

  def next(tiles:Map[(Int,Int), Int]) =
    addWhiteNeighbors(tiles).map( (tile,status) => {
      val black = countBlackNeighbors(tile,tiles)
      status match {
        case 1 if (black == 0 || black > 2) => (tile, 0)
        case 0 if (black == 2) => (tile, 1)
        case _ => (tile, status)
      }
    })

  def countBlackTiles(input : LazyList[String]) =
    val tiles = getTilesStatus(input)
    val nTiles100 = (1 to 100).foldLeft(tiles)((acc,_) => next(acc))
    nTiles100.values.sum

  def main(args: Array[String]): Unit = {
    println("part1=" + countFlippedTiles(fullInput))
    println("part2=" + countBlackTiles(fullInput))
  }
}
