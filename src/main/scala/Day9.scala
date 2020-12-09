object Day9 {
  import scala.io.Source
  val fullInput = Source.fromResource("day9-input.txt").getLines.toArray.toList.map(_.toLong)

  val smallInput = Array(
    35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576).map(_.toLong).toList

    def isException(numberToFind:Long, previousNumbers: List[Long]) =
      previousNumbers.combinations(2).filter {
        case v1::v2::Nil => v1 + v2 == numberToFind
      }.nextOption

    def findException(numbers:List[Long], preambuleSize:Int) =
      numbers.sliding(preambuleSize+1).flatMap((window) => {
        val previousNumbers = window.slice(0, preambuleSize)
        val numberToFind = window.last
        isException(numberToFind,previousNumbers) match {
          case Some(_) => None
          case None => Some(numberToFind)
        }
      }).take(1).toList(0)

    def findContiguousSeq(target:Long, numbers:List[Long]) : List[Long] = 
      if (numbers.length > 1) {
        val seq = (for (i<- (2 to numbers.length)) yield  numbers.sliding(i)).to(LazyList).flatten
        val solution = seq.filter(_.sum == target).take(1).toList
        solution match {
          case Nil => findContiguousSeq(target, numbers.tail)
          case s::_ => s
        }
      } else List()

    def ComputeEncrytionWeakness(numbers:List[Long], preambuleSize:Int) = {
      val target = findException(numbers, preambuleSize)
      val seq = findContiguousSeq(target, numbers)
      seq.min + seq.max
    }
    def main(args: Array[String]): Unit = {
      val target = findException(fullInput,25)
      println("part1=" + target)
      println("part2=" + ComputeEncrytionWeakness(fullInput,25))
    }
  
}