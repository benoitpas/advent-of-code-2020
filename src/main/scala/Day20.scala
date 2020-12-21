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
  type LocationMap = Map[(Int,Int),Tile]

  def findNeighbors(x:Int, y:Int, locations:LocationMap, remaingTiles:LazyList[Tile]) :(LocationMap,LazyList[Tile]) = {
    val xytile = locations((x,y))

    def remove(tile:LazyList[Tile], id:Int) = tile.filter(_.id != id)

    def findNeighbor(neighBorX: Int, neighBorY: Int, compare: (Tile,Tile) => Boolean, 
      locations:LocationMap, remaingTiles:LazyList[Tile]) :
      (LocationMap, LazyList[Tile]) = 
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

  def computeLocations(input:LazyList[String]) = {
    val sections = Day16.extractSections(input)
    val tiles = sections.map(Tile(_))
    val firstTile = tiles.head
    val initialLocations = Map((0,0)->firstTile)
    val remaingTiles = tiles.tail
    findNeighbors(0,0,initialLocations,remaingTiles)._1
  }

  def getPoints(locations: Map[(Int,Int), Tile]) =
    locations.toList.filter(_._2.id > 0).map(_._1)

  def getCorners(points: List[(Int,Int)]) =
    val allX = points.map(_._1)
    val allY = points.map(_._2)
    (allX.min, allY.min, allX.max, allY.max)


  def productCorners(input: LazyList[String]) = {
    val locations = computeLocations(input)
    val points = getPoints(locations)
    val (minX, minY, maxX, maxY) = getCorners(points)

    def idLong(x:Int,y:Int) = locations(x,y).id.toLong

    idLong(minX,minY) * idLong(minX,maxY) * idLong(maxX,minY) * idLong(maxX,maxY)
  }

  def removeBorders(tile: Tile) =
    tile.grid.tail.dropRight(1).map(_.tail.dropRight(1))

  def stitchImages(locations: LocationMap) = {
    val points = getPoints(locations)
    val (minX, minY, maxX, maxY) = getCorners(points)
    val tiles = for(y <- minY to maxY) yield {
      for(x <- minX to maxX) yield removeBorders(locations(x,y))
    }

    val tileSize = tiles(0)(0).size
    val initialVector = (1 to tileSize).map((i)=>Vector[Char]()).toVector
    val grid = tiles.flatMap((tilesRow) => tilesRow.foldLeft(initialVector)(
      (acc,tile) => (acc zip tile).map( _ ++ _)
    )).toVector

    Tile(0, grid)
  }

  val monsterPattern = Vector(
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ")
  val monsterMaxX = monsterPattern(0).size
  val monsterMaxY = monsterPattern.size

  def findSeaMonster(grid: Vector[Vector[Char]]) = {

    def fits(monsterX:Int,monsterY:Int) =
      (for (x <- 0 until monsterMaxX;y <- 0 until monsterMaxY) yield (x,y)).foldLeft(true)(
        (acc,pos) => acc && (monsterPattern(pos._2)(pos._1) match {
          case ' ' => true
          case '#' => grid(monsterY+pos._2)(monsterX+pos._1) == '#'
        }))

    val possibleLocation = (for
      (x <- 0 until grid(0).size - monsterMaxX;
       y <- 0 until grid.size - monsterMaxY) yield (x,y))

    possibleLocation.filter(fits)
  }

  def waterRoughness(input: LazyList[String]) = {
    val locations = computeLocations(input)
    val points = getPoints(locations)
    val (minX, minY, maxX, maxY) = getCorners(points)

    val grid = stitchImages(locations)
    val (monsterLocations, tileWithMonster) = grid.allCombinations.map(
      (tile) => (findSeaMonster(tile.grid), tile)
    ).filter(_._1.length > 0).take(1)(0)

    def addMonster(grid: Vector[Vector[Char]], position:(Int,Int)) =
      grid.take(position._2)
      ++ {
        val matchingRows = grid.slice(position._2, position._2+monsterMaxY)
        val z = grid.slice(position._2, position._2+monsterMaxY) zip monsterPattern
        z.map((gridRow, monsterRow) => {
          // Insert the sea monster
          val insertedMonster = (gridRow.slice(position._1, position._1 + monsterRow.size) zip monsterRow).map {
            // case will fail to detect incorrectly detected monsters
            case (g,' ') => g
            case ('#','#') => 'O'
          }
          // Put the row back together
          gridRow.take(position._1) ++ insertedMonster ++ gridRow.slice(position._1 + monsterRow.size, gridRow.size)
        })
      }
      ++ grid.slice(position._2+monsterMaxY, grid.size)

    val gridWithMonster = monsterLocations.foldLeft(tileWithMonster.grid)(addMonster)
    gridWithMonster.flatten.count(_ == '#')
  }

  def main(args: Array[String]): Unit = {
    println("part1=" + productCorners(fullInput))
    println("part2=" + waterRoughness(fullInput))
  }
}