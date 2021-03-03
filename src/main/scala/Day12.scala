object Day12 {
  import scala.io.Source
  val fullInput = Source.fromResource("day12-input.txt").getLines.toList

  val smallInput = List("F10",
  "N3",
  "F7",
  "R90",
  "F11")

  type Position = (Int, Int)
  type State1 = (Char, Position)

  def go(position:Position, direction:Char, v:Int) : Position = 
    direction match
      case 'N' => (position._1, position._2 - v)
      case 'S' => (position._1, position._2 + v)
      case 'E' => (position._1 + v, position._2)
      case 'W' => (position._1 - v, position._2)

  val clockWiseDirection = List('N', 'E', 'S', 'W')
  val headings = clockWiseDirection.toSet

  def rotate(direction: Char, angle:Int, turn:Char) =
    val clockWise = (turn match {
      case 'L' => -1
      case 'R' => 1
    }) * angle / 90
    val nbDirection = clockWiseDirection.length
    val index = clockWiseDirection.indexOf(direction)
    val nextIndex = (index + clockWise +nbDirection) % nbDirection
    clockWiseDirection(nextIndex)

  def next1(state:State1, cmd:String) = 
    val p = cmd.splitAt(1)
    (p._1(0), p._2.toInt, state._1) match
      case ('F', v, d) => (d, go(state._2, d, v))
      case ('N', v, d) => (d, go(state._2, 'N', v))
      case ('S', v, d) => (d, go(state._2, 'S', v))
      case ('E', v, d) => (d, go(state._2, 'E', v))
      case ('W', v, d) => (d, go(state._2, 'W', v))
      case ('R', a, d) => (rotate(d, a, 'R'), state._2)
      case ('L', a, d) => (rotate(d, a, 'L'), state._2)

  val iPos1 : State1 = ('E',(0,0))

  def iterate1(input:List[String]) =
    val lastState = input.foldLeft(iPos1)(next1)
    math.abs(lastState._2._1) + math.abs(lastState._2._2)


  case class State2(ship: Position, waypoint: Position) {
    def moveShip(v:Int) = State2(
      ( ship._1 + waypoint._1 * v,
        ship._2 + waypoint._2 * v),
      waypoint)

    def moveWaypoint(h: Char, v:Int) = State2(
      ship,
      h match {
        case 'N' => (waypoint._1, waypoint._2 - v)
        case 'S' => (waypoint._1, waypoint._2 + v)
        case 'E' => (waypoint._1 - v, waypoint._2)
        case 'W' => (waypoint._1 + v, waypoint._2)
      }
    )

    def rotateWaypoint(v: Int) = State2(
      ship,
      v match {
        case  -90 => (-waypoint._2,  waypoint._1)
        case   90 => ( waypoint._2, -waypoint._1)
        case  180 => (-waypoint._1, -waypoint._2)
        case -180 => (-waypoint._1, -waypoint._2)
        case -270 => ( waypoint._2, -waypoint._1)
        case  270 => (-waypoint._2,  waypoint._1)
      }
    )
  }

  val iPos2 = State2((0,0),(-10,-1))

  def next2(state:State2, cmd:String) = 
    val (cmdLetter, cmdValue) = cmd.splitAt(1)
    (cmdLetter(0), cmdValue.toInt) match
      case ('F',v) => state.moveShip(v)
      case (heading,v) if (headings.contains(heading)) => state.moveWaypoint(heading, v)
      case ('L', v) => state.rotateWaypoint(-v)
      case ('R', v) => state.rotateWaypoint(v)

  def iterate2(input:List[String]) =
    val lastState = input.foldLeft(iPos2)(next2)
    math.abs(lastState.ship._1) + math.abs(lastState.ship._2)
    
  def main(args: Array[String]): Unit = {
    println("part1=" + iterate1(fullInput))
    println("part1=" + iterate2(fullInput))
  }
}