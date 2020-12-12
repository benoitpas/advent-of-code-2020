object Day12 {
  import scala.io.Source
  val fullInput = Source.fromResource("day12-input.txt").getLines.toList

  val smallInput = List("F10",
  "N3",
  "F7",
  "R90",
  "F11")

  type Position = (Int, Int)
  type State = (Char, Position)

  def go(position:Position, direction:Char, v:Int) : Position = 
    direction match
      case 'N' => (position._1, position._2 - v)
      case 'S' => (position._1, position._2 + v)
      case 'E' => (position._1 + v, position._2)
      case 'W' => (position._1 - v, position._2)

  def rotate(direction: Char, angle:Int, turn:Char) =
    val clockWiseDirection = List('N', 'E', 'S', 'W')
    val clockWise = (turn match {
      case 'L' => -1
      case 'R' => 1
    }) * angle / 90
    val nbDirection = clockWiseDirection.length
    val index = clockWiseDirection.indexOf(direction)
    val nextIndex = (index + clockWise +nbDirection) % nbDirection
    clockWiseDirection(nextIndex)

  def next(state:State, cmd:String) = 
    val p = cmd.splitAt(1)
    (p._1(0), p._2.toInt, state._1) match
      case ('F', v, d) => (d, go(state._2, d, v))
      case ('N', v, d) => (d, go(state._2, 'N', v))
      case ('S', v, d) => (d, go(state._2, 'S', v))
      case ('E', v, d) => (d, go(state._2, 'E', v))
      case ('W', v, d) => (d, go(state._2, 'W', v))
      case ('R', a, d) => (rotate(d, a, 'R'), state._2)
      case ('L', a, d) => (rotate(d, a, 'L'), state._2)

  val iPos = State('E',(0,0))

  def iterate(input:List[String]) =
    val lastState = input.foldLeft(iPos)(next)
    math.abs(lastState._2._1) + math.abs(lastState._2._2)

  def main(args: Array[String]): Unit = {
    println("part1=" + iterate(fullInput))
  }
}