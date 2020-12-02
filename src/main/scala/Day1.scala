
object Day1 {

  import scala.io.Source
   
  val input = Source.fromResource("day1-input.txt").getLines

  val totalSum = 2020

  // Interesting find: 'Stream' has been deprecated and replaced by 'LazyList'
  def findPair(numbers : List[Int]) = {
    val pairs = for(n1 <- numbers.to(LazyList);n2<-numbers.to(LazyList)) yield (n1,n2)
    
    pairs.filter((p) => p._1+p._2 == totalSum).take(1).toList
  }

  def findTriple(numbers : List[Int]) = {
    val pairs = for(
      n1 <- numbers.to(LazyList);
      n2<-numbers.to(LazyList);
      n3<-numbers.to(LazyList)
      ) yield (n1,n2,n3)
    
    pairs.filter((t) => t._1+t._2+t._3 == totalSum).take(1).toList
  }


  def main(args: Array[String]): Unit = {
    val numbers = for(i <- input.toList) yield i.toInt

    val pair = findPair(numbers)(0)
    println(s"Pair product: ${pair._1 * pair._2}")

    val triple = findTriple(numbers)(0)
    println(s"Triple product: ${triple._1 * triple._2 * triple._3}")
  }
}
