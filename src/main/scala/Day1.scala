
object Day1 {

  import scala.io.Source
   
  val input = Source.fromResource("day1-input.txt").getLines

  // Interesting find: 'Stream' has been deprecated and replaced by 'LazyList'
  def findPair(numbers : List[Int]) = {
    val pairs = for(n1 <- numbers.to(LazyList);n2<-numbers.to(LazyList)) yield (n1,n2)
    
    pairs.filter((p) => p._1+p._2 == 2020).take(1).toList
  }

  def main(args: Array[String]): Unit = {
    val numbers = for(i <- input.toList) yield i.toInt
    val pair = findPair(numbers)(0)
    println(pair._1 * pair._2)
  }
}
