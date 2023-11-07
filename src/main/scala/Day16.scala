object Day16 {
  import scala.io.Source
  val fullInput = Source.fromResource("day16-input.txt").getLines.to(LazyList)

  val smallInput1 = LazyList(
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
    "38,6,12")

  val smallInput2 = LazyList(
    "class: 0-1 or 4-19",
    "row: 0-5 or 8-19",
    "seat: 0-13 or 16-19",
    "",
    "your ticket:",
    "11,12,13",
    "",
    "nearby tickets:",
    "3,9,18",
    "15,1,5",
    "5,14,9")

  type Sections = LazyList[LazyList[String]]

  def extractSections(input:LazyList[String]) = input.foldRight(LazyList[LazyList[String]]())(
    (s:String, acc:LazyList[LazyList[String]]) => (acc,s.trim) match  {
      case (LazyList(), "") => acc
      case (LazyList(), s) => LazyList(LazyList(s))
      case (_,"") => LazyList[String]()#::acc
      case (head#::tail,s) => (s#::head)#::tail
  })

  def parseInput(input: LazyList[String]) = {
  
    val sections = extractSections(input)
    val fieldsRegex = s"([\\w\\s]+):\\s+(\\d+)-(\\d+)\\s+or\\s+(\\d+)-(\\d+)".r
    val fields = sections(0).map(fieldsRegex.findFirstMatchIn(_) match {
      case Some(m) =>(m.group(1) -> List(
        (m.group(2).toInt, m.group(3).toInt),
        (m.group(4).toInt, m.group(5).toInt)))
    }).toMap

    def ticketToRow(s:String) = s.split(",").map(_.trim.toInt)
    val ticket = ticketToRow(sections(1).tail.head)

    val nearbyTickets = sections(2).tail.map(ticketToRow).toArray
    (fields, nearbyTickets, ticket)
  }

  def checkIntervals(v:Int, intervals: List[(Int,Int)]) = intervals.foldLeft(false)
    ((acc,interval) => acc || (interval._1 <= v && v <= interval._2))

  def checkFields(v:Int, fields : Map[String, List[(Int,Int)]]) = 
    fields.values.foldLeft(false)(
      (acc,intervals) => ((intervals(0)._1 <= v && v <= intervals(0)._2)
        || (intervals(1)._1 <= v && v <= intervals(1)._2) || acc))

  def addInvalidValues(tickets : Array[Array[Int]], fields : Map[String, List[(Int,Int)]]) = {
    val values = for( 
      row <- tickets;
      n <- row if (!checkFields(n, fields))) yield n
    
    values.sum
  }

  def mapFields(tickets : Array[Array[Int]], fields : Map[String, List[(Int,Int)]]) = {
    def checkTicket(ticket:Array[Int]) =
       ticket.foldLeft(true)((acc,v) => checkFields(v,fields) && acc)

    val goodTickets = tickets.foldLeft(List[Array[Int]]())(
      (acc, ticket) => if (checkTicket(ticket)) ticket::acc else acc).toArray

    def check(column: IndexedSeq[Int], intervals: List[(Int,Int)]) = 
      column.foldLeft(true)((acc,v) => checkIntervals(v,intervals) && acc)

    val possibleMapping = (0 to goodTickets(0).length - 1).map((columnIndex) => {
      fields.flatMap((name,intervals) => {
        val column = for (i <- (0 to goodTickets.length - 1)) yield goodTickets(i)(columnIndex)
        if ( check(column, intervals)) List(name) else List()
      })
    })

    val iPossibleMapping = possibleMapping.zipWithIndex
    def loop(iPossibleMapping :IndexedSeq[(Iterable[String], Int)], mapping:Map[String,Int]): Map[String,Int] =
      if (iPossibleMapping.length == 0) mapping
      else {
        val (field, index) = iPossibleMapping.find(_._1.size == 1) match {
          case Some((field::_, index)) => (field, index)
        }
        val newPossibleMapping = iPossibleMapping.map(
          (fields,index) => (fields.filter(_ != field), index)).filter(_._1.size > 0)
        loop(newPossibleMapping, mapping + (field -> index))
      }

    loop(iPossibleMapping, Map[String,Int]())
    
  }

  def departureFieldsProduct(tickets: Array[Array[Int]], 
    fields: Map[String, List[(Int,Int)]], myTicket: Array[Int]) =
    val allFields = mapFields(tickets, fields)
    val departureFields = allFields.filter((pair) => "departure .*".r.matches(pair._1))
    departureFields.values.map(myTicket(_).toLong).reduce(_ * _)

  def main(args: Array[String]): Unit = {
    val (fields, tickets, myTicket) = parseInput(fullInput)
    println("part1=" + addInvalidValues(tickets, fields))
    println("part2=" + departureFieldsProduct(tickets, fields, myTicket))
  }
}