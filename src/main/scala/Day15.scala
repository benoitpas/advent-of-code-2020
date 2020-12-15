object Day15 {

  val smallList = List(0,3,6)
  val fullList = List(2,0,6,12,1,3)

  def toLastOccurrences(l:List[Int]) =
    l.zipWithIndex.foldLeft(Map[Int,List[Int]]())((acc,e) => {
      val newPair = acc.get(e._1) match {
        case Some(l) => (e._2 + 1)::l
        case None => List(e._2 + 1)
      }
      acc ++ List((e._1, newPair))
    })
  
  def iterate(initialList:List[Int]) = {
    def next(n:Int, i:Int, m: Map[Int,List[Int]]) = {
      val (lastIndex, newPair) = m.get(n) match {
        case Some(l) => (l.head, (i::l).take(2))
        case None => (i, List(i))
      }
      (i-lastIndex, m ++ List(n -> newPair))
    }

    def loop(n:Int, i:Int, m: Map[Int,List[Int]]) : LazyList[(Int,Int)] = {
      val (nextN, newMap) = next(n,i,m)
      (n, i)#::loop(nextN, i+1, newMap)
    }
      
    loop(initialList.last, initialList.length, toLastOccurrences(initialList.take(initialList.length - 1)))
  }

  def find2020(initialList:List[Int]) =
    iterate(initialList).filter(_._2 == 2020).take(1).toList(0)._1

  def main(args: Array[String]): Unit = {
    println("part1=" + find2020(fullList))
  }
}