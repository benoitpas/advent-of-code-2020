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

  val verySmallInput = LazyList(
    "Player 1:",
    "43",
    "19",
    "",
    "Player 2:",
    "2",
    "29",
    "14")

  type Decks = (List[Int],List[Int])
  @scala.annotation.tailrec
  def play1(cards1:List[Int], cards2:List[Int]) : Decks =
    if (cards1.length == 0 || cards2.length == 0) (cards1,cards2)
    else
      math.signum(cards1.head - cards2.head) match {
        case 1 => play1(cards1.tail ++ List(cards1.head,cards2.head), cards2.tail)
        case -1 => play1(cards1.tail, cards2.tail ++ List(cards2.head, cards1.head))
        case _ => throw Exception("Cards should not be equal")
      }
 
  def scoreFromDecks(decks: Decks) =
    val winningCards = if (decks._2.length > 0) decks._2 else decks._2
    (winningCards zip (winningCards.length to 1 by -1)).map((p) => p._1 * p._2).sum

  def score1(input:LazyList[String]) =
    val sections = Day16.extractSections(input)
    val cards1 = sections(0).tail.map(_.toInt).toList
    val cards2 = sections(1).tail.map(_.toInt).toList
    val decks = play1(cards1,cards2)
    scoreFromDecks(decks)

  def recursiveGame(cards1:List[Int], cards2:List[Int]) =
    cards1.tail.length >= cards1.head && cards2.tail.length >= cards2.head

  def play2(cards1:List[Int], cards2:List[Int], games:Set[Decks]) : Decks =
    //println(cards1.toString+"\t"+cards2+"\t"+games)
    lazy val newGames = games ++ Set((cards1,cards2))
    if (cards1.length == 0 || cards2.length == 0|| games.contains((cards1,cards2))) (cards1,cards2)
    else
      val winningDeck =
        if (recursiveGame(cards1,cards2))
          play2(cards1.tail.take(cards1.head), cards2.tail.take(cards2.head), newGames) match
            case (Nil,_) => -1
            case _ => 1
        else math.signum(cards1.head - cards2.head)

      winningDeck match {
        case 1 => play2(cards1.tail ++ List(cards1.head,cards2.head), cards2.tail, newGames)
        case -1 => play2(cards1.tail, cards2.tail ++ List(cards2.head, cards1.head), newGames)
        case _ => throw Exception("Cards should not be equal")
      }

  def score2(input:LazyList[String]) =
    val sections = Day16.extractSections(input)
    val cards1 = sections(0).tail.map(_.toInt).toList
    val cards2 = sections(1).tail.map(_.toInt).toList
    val decks = play2(cards1,cards2, Set())
    scoreFromDecks(decks)

  def main(args: Array[String]): Unit = {
    println("part1=" + score1(fullInput))
    println("part2=" + score2(fullInput))
  }
}