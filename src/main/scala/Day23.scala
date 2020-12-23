object Day23 {

  val exampleInput = "389125467"
  val puzzleInput = "284573961"

  def inputToList(s:String) = s.toList.map(_.toString.toInt)

  def oneRound(list:List[Int]) = 
    val listLength = list.length
    def modulo(i:Int) = (i + listLength - 1) % listLength + 1

    val n = list(0)
    val toInsert = list.slice(1,4)
    val remain = list.head::list.drop(4)
    def findIndexForInsert(n:Int) : Int =
      val previousN = modulo(n-1)
      remain.indexOf(previousN) match {
        case -1 => findIndexForInsert(previousN)
        case i => i
      }
    val indexForInsert = findIndexForInsert(n)
    val r = remain.slice(0,indexForInsert + 1)++ toInsert ++ remain.slice(indexForInsert + 1, remain.length)
    r.tail ++ List(r.head)

  def shiftRight(l:List[Int], n:Int) =
    (1 to n).foldLeft(l)((acc,_) => l.last::l.dropRight(1))

  def nRound(list: List[Int], count:Int) =
    val r = (1 to count).foldLeft(list)((acc,_) => oneRound(acc))
    val nbShift = count % list.length
    shiftRight(r, nbShift)

  def finalOrder(l:List[Int]) : List[Int] = l match {
    case 1::tail => tail
    case head::tail => finalOrder(tail ++ List(head))
  }

  def labels(input:String) = 
    val l = inputToList(input)
    finalOrder(nRound(l,100)).mkString("")

  def main(args: Array[String]): Unit = {
    println("part1=" + labels(puzzleInput))
  }
}