object Day22 {
  import scala.io.Source
  val fullInput = Source.fromResource("day22-input.txt").getLines.to(LazyList)

  val smallInput = LazyList(
  "Player 1:",
  "9",
  "2",
  "6",
  "3",
  "1",
  "",
  "Player 2:",
  "5",
  "8",
  "4",
  "7",
  "10")

  @scala.annotation.tailrec
  def play(cards1:List[Int], cards2:List[Int]) : (List[Int], List[Int]) = 
    if( cards1.length == 0 || cards2.length == 0) (cards1,cards2)
    else  math.signum(cards1.head - cards2.head) match {
      case 1 => play(cards1.tail ++ List(cards1.head,cards2.head), cards2.tail)
      case -1 => play(cards1.tail, cards2.tail ++ List(cards2.head, cards1.head))
      case _ => throw Exception("Cards should not be equal")
    }
 
  def score(input:LazyList[String]) = 
    val sections = Day16.extractSections(input)
    val cards1 = sections(0).tail.map(_.toInt).toList
    val cards2 = sections(1).tail.map(_.toInt).toList
    val gameResult = play(cards1,cards2)

    val winningCards = if (gameResult._1.length > 0) gameResult._1 else gameResult._2
    (winningCards zip (winningCards.length to 1 by -1)).map((p) => p._1 * p._2).sum

  def main(args: Array[String]): Unit = {
   println("part1=" + score(fullInput))
  }
}