object Day16 {
  import scala.io.Source
  val fullInput = Source.fromResource("day16-input.txt").getLines.to(LazyList)


  val smallInput = List(
    "class: 1-3 or 5-7",
    "row: 6-11 or 33-44",
    "seat: 13-40 or 45-50",
    "",
    "your ticket:",
    "7,1,14",
    "",
    "nearby tickets:",
    "7,3,47",
    "40,4,50",
    "55,2,20",
    "38,6,12").to(LazyList)

  type Sections = LazyList[LazyList[String]]

  def parseInput(input: LazyList[String]) = {
  
    val sections = input.foldRight(LazyList[LazyList[String]]())(
      (s:String, acc:LazyList[LazyList[String]]) => (acc,s.trim) match  {
        case (Nil,"") => acc
        case (Nil,s) => LazyList(LazyList(s))
        case (_,"") => LazyList[String]()#::acc
        case (head#::tail,s) => (s#::head)#::tail
    })

    val fieldsRegex = s"(\\w+):\\s+(\\d+)-(\\d+)\\s+or\\s+(\\d+)-(\\d+)".r
    val fields = sections(0).map(fieldsRegex.findFirstMatchIn(_) match {
      case Some(m) =>(m.group(1) -> List(
        (m.group(2).toInt, m.group(3).toInt),
        (m.group(4).toInt, m.group(5).toInt)))
    }).toMap

    val nearbyTickets = sections(2).tail.map(_.split(",").map(_.trim.toInt)).toArray
    (fields, nearbyTickets)
  }


  def addInvalidValues(tickets : Array[Array[Int]], fields : Map[String, List[(Int,Int)]]) = {
    def checkFields(v:Int) = fields.values.foldLeft(false)(
      (acc,intervals) => ((intervals(0)._1 <= v && v <= intervals(0)._2)
        || (intervals(1)._1 <= v && v <= intervals(1)._2) || acc))

    val values = for( 
      row <- tickets;
      n <- row if (!checkFields(n))) yield n
    
    values.sum
  }

  def main(args: Array[String]): Unit = {
    val r2 = parseInput(fullInput)
    println("part1=" + addInvalidValues(r2._2,r2._1))
  }
}