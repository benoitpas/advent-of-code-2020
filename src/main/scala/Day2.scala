object Day2 {

  import scala.io.Source
   
  val input = Source.fromResource("day2-input.txt").getLines.to(LazyList)

  val passwords = List("1-3 a: abcde","1-3 b: cdefg","2-9 c: ccccccccc")

  def row = "(\\d+)-(\\d+) (\\w): (\\w+)".r

  // (min:Int, max:Int, letter:Char, password:String)
  def extractRow(line: String): (Int,Int,Char,String) = {
    row.findFirstMatchIn(line) match {
      case Some(m) if (m.groupCount == 4) => (m.group(1).toInt,m.group(2).toInt, 
        m.group(3).toCharArray()(0), m.group(4) )
      case _ => (0,0,'z',"") // to fail the tests to find non matching patterns
    }
  }

  def checkPassword1(min:Int, max:Int, letter:Char, password:String) : Int = {
    val nbLetter = password.toList.filter(letter == _).length
    if (min <= nbLetter && nbLetter <= max) 1 else 0
  }

  def checkPassword1(input : Seq[String]) : Seq[Int] 
    = input.map(extractRow).map(m => checkPassword1(m._1, m._2, m._3, m._4))

  def checkPassword2(i1:Int, i2:Int, letter:Char, password:String) : Int = {
    val c1 = password(i1-1) == letter 
    val c2 = password(i2-1) == letter 
    if ((c1 && !c2) || (!c1 && c2)) 1 else 0
  }
  
  def checkPassword2(input : Seq[String]) : Seq[Int] 
    = input.map(extractRow).map(m => checkPassword2(m._1, m._2, m._3, m._4))

  def main(args: Array[String]): Unit = {
    val r1 = checkPassword1(input)

    val s1 = r1.sum
    println(s"s1=${s1}")

    val r2 = checkPassword2(input)

    val s2 = r2.sum
    println(s"s2=${s2}")
  }
}