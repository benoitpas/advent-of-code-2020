object Day20 {
  import scala.io.Source
  val fullInput = Source.fromResource("day20-input.txt").getLines.to(LazyList)
  val smallInput = Source.fromResource("day20-smallInput.txt").getLines.to(LazyList)

  case class Tile(id: Int, grid: Vector[Vector[Char]]) {
    lazy val hFlip = Tile(id, grid.reverse)
    lazy val vFlip = Tile(id, grid.map(_.reverse))
    lazy val dFlip = {
      val newGrid = (for (i<- 0 until grid.length) yield
        (for(j<-0 until grid.length) yield grid(j)(i)).toVector).toVector
      Tile(id, newGrid)
    }

    lazy val allCombinations = {
      LazyList(this, vFlip, hFlip, hFlip.vFlip, dFlip, dFlip.vFlip, dFlip.hFlip, dFlip.hFlip.vFlip)
    }

    lazy val top = grid.head
    lazy val bottom = grid.last
    lazy val left = for (i<-0 until grid.length) yield grid(i).head
    lazy val right = for (i<-0 until grid.length) yield grid(i).last

    def print = {
      s"Tile ${id}\n"
      + grid.map(_.mkString).mkString("\n")
    }
  }

  object Tile {
    def apply(strings: LazyList[String]):Tile = {
      val id = strings(0).split(" ")(1).split(":")(0).toInt
      val grid = strings.drop(1).map(_.toVector).toVector
      Tile(id, grid)
    }

    val borderTile = Tile(-1,Vector[Vector[Char]]())
  }

  type Location = ((Int,Int),Tile)

  def findNeighbors(x:Int, y:Int, locations:Map[(Int,Int),Tile], remaingTiles:LazyList[Tile]) :(Map[(Int,Int),Tile],LazyList[Tile]) = {
    val xytile = locations((x,y))

    def remove(tile:LazyList[Tile], id:Int) = tile.filter(_.id != id)

    def findNeighbor(neighBorX: Int, neighBorY: Int, compare: (Tile,Tile) => Boolean, 
      locations:Map[(Int,Int),Tile], remaingTiles:LazyList[Tile]) : 
      (Map[(Int,Int),Tile], LazyList[Tile]) = 
        locations.get((neighBorX, neighBorY)) match {
          case Some(t) => (locations, remaingTiles)
          case None => {
            val neighBor = remaingTiles.flatMap(_.allCombinations).foldLeft(None:Option[Location])(
              (acc, tile) => acc match {
                case None if (compare(xytile,tile)) => Some((neighBorX,neighBorY) -> tile)
                case _ => acc
              })
              neighBor match {
                case Some(location) => {
                  val locationsNeighbor = locations ++ Map(location)
                  val remaingTilesNeighbor = remove(remaingTiles, location._2.id)
                  findNeighbors(neighBorX, neighBorY, locationsNeighbor, remaingTilesNeighbor)
                }
               case None => (locations ++ Map((neighBorX,neighBorY) -> Tile.borderTile), remaingTiles)
            }
        }
      }

    // Right
    val xRight = x + 1
    val yRight = y
    val (locationsRight, remaingTilesRight) = findNeighbor(xRight, yRight, 
      (currentTile: Tile, rightTile: Tile) => currentTile.right == rightTile.left,
      locations,remaingTiles)

    // Bottom
    val xBottom = x
    val yBottom = y + 1
    val (locationsBottom, remaingTilesBottom) = findNeighbor(xBottom, yBottom, 
      (currentTile: Tile, bottomTile: Tile) => currentTile.bottom == bottomTile.top,
      locationsRight,remaingTilesRight)

    // Left
    val xLeft = x - 1
    val yLeft = y
    val (locationsLeft, remaingTilesLeft) = findNeighbor(xLeft, yLeft, 
      (currentTile: Tile, leftTile: Tile) => currentTile.left == leftTile.right,
      locationsBottom,remaingTilesBottom)

    // top
    val xTop = x
    val yTop = y - 1
    val (locationsTop, remaingTilesTop) = findNeighbor(xTop, yTop, 
      (currentTile: Tile, topTile: Tile) => currentTile.top == topTile.bottom,
      locationsLeft,remaingTilesLeft)

    (locationsTop, remaingTilesTop)
  }

  def productCorners(input:LazyList[String]) = {
    val sections = Day16.extractSections(input)
    val tiles = sections.map(Tile(_))
    val firstTile = tiles.head
    val initialLocations = Map[(Int,Int),Tile]((0,0)->firstTile)
    val remaingTiles = tiles.tail
    val locations = findNeighbors(0,0,initialLocations,remaingTiles)._1
    val points = locations.toList.filter(_._2.id > 0).map(_._1)
    val minX = points.map(_._1).min
    val maxX = points.map(_._1).max
    val minY = points.map(_._2).min
    val maxY = points.map(_._2).max

    def idLong(x:Int,y:Int) = locations(x,y).id.toLong

    idLong(minX,minY) * idLong(minX,maxY) * idLong(maxX,minY) * idLong(maxX,maxY)
  }

  def main(args: Array[String]): Unit = {
    println("part1=" + productCorners(fullInput))
  }
}