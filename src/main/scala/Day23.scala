object Day23 {

  val exampleInput = "389125467"
  val puzzleInput = "284573961"

  def inputToList(s:String) : LazyList[Int] = s.to(LazyList).map(_.toString.toInt)

  def inputToVector(initialList:LazyList[Int], count:Int) : Array[Int] =
    val l = initialList ++ (initialList.length  + 1 to count)
    val pairs = l zip (l.tail ++ List(l.head))
    pairs.sortBy(_._1).map(_._2).toArray

  def toList(mapping:Array[Int]) : LazyList[Int] =
    mapping.foldLeft(LazyList(1))((acc,next) => acc ++ LazyList(mapping(acc.last -1)))

  def finalOrder(l: LazyList[Int]) = l.tail.dropRight(1).mkString("")

  def toStr(mapping: Array[Int]) =
    val l = toList(mapping)
    val extract = l.take(10) ++ List("","","") ++ l.takeRight(10)
    "(" + extract.mkString(",") +")"

  def modulo2(value: Int, mapping: Array[Int]) : Int = (value + mapping.length) % mapping.length

  def iterate2(l:LazyList[Int], size: Int, count:Int) : Array[Int]=
    val firstValue = l.head
    val mapping = inputToVector(l, size)
    (1 to count).foldLeft(firstValue)((acc,i) => {
      nextRound(acc, mapping)
      mapping(modulo2(acc - 1, mapping))
    })
    mapping

  def iterate(l:LazyList[Int], size: Int, count:Int) : LazyList[Int]=
    toList(iterate2(l,size,count))

  def nextRound(value: Int, mapping: Array[Int]) = {
    def modulo(value:Int) : Int = modulo2(value, mapping)
    def getNext(value: Int) = mapping( modulo(value - 1))
    def setNext(target : Int, value:Int) = mapping(modulo(target - 1)) = value

    val next1 = getNext(value)
    val next2 = getNext(next1)
    val next3 = getNext(next2)
    val valueToInsert = (value - 1 to value - 4 by -1).map(i => modulo(i - 1) + 1).filter(v => (v != next1 && v != next2 && v != next3))(0)
    setNext(value, getNext(next3))
    setNext(next3, getNext(valueToInsert))
    setNext(valueToInsert, next1)
  }

  def labels(input:String) =
    val l = inputToList(input)
    finalOrder(iterate(l, l.length, 100)).mkString("")

  def product(input:String) =
    val l = inputToList(input)
    val m = iterate2(l, 1000000,10000000)
    m(0).toLong * m(m(0) - 1).toLong

  def main(args: Array[String]): Unit = {
    println("part1=" + labels(puzzleInput))
    println("part2=" + product(puzzleInput))
 }
}