object Day10 {
  import scala.io.Source
  val fullInput = Source.fromResource("day10-input.txt").getLines.toList.map(_.toInt)

  val verySmallInput = List(16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4)

  val smallInput = List(28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3)

  def joltDifferencesMultiplied(adapters:List[Int]) = {
    val l = adapters.sorted
    val differences = (l.appended(l.last+3) zip 0::l) map (_ - _)

    def countDifferences(d:Int) = differences.filter(_ == d).length

    countDifferences(1) * countDifferences(3)
  }

  def countCombinations(adapters:List[Int]) = {
    val l = adapters.sorted
    val l2 = 0::l.appended(l.last+3)
    val counts = l2.foldRight(List[(Int,Long)]())((elem,acc) => acc match {
      case Nil => List((elem, 1L))
      case h::Nil => (elem, h._2)::h::Nil
      case h1::h2::Nil => {
        val inc = if ((h2._1 - elem) < 3) h2._2 else 0
        (elem,h1._2 + inc)::h1::h2::Nil
      }
      case h1::h2::h3::tail => {
        val inc = 
          (if ((h2._1 - elem) <= 3) h2._2 else 0) +
          (if ((h3._1 - elem) <= 3) h3._2 else 0)
        (elem,h1._2 + inc)::h1::h2::h3::tail
      }
      case h::tail => (elem,h._2+1)::h::tail
    })
    counts.head._2
  }


  def main(args: Array[String]): Unit = {
    println("part1="+joltDifferencesMultiplied(fullInput))
    println("part2="+countCombinations(fullInput)) 
  }
}