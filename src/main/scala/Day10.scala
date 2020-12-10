object Day10 {
  import scala.io.Source
  val fullInput = Source.fromResource("day10-input.txt").getLines.toList.map(_.toInt)

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

  def main(args: Array[String]): Unit = {
    println(joltDifferencesMultiplied(smallInput))    
    println(joltDifferencesMultiplied(fullInput))    
  }
}