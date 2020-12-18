object Day18 {
  import scala.io.Source
  val fullInput = Source.fromResource("day18-input.txt").getLines.to(LazyList)

  // Part 1
  def upToClosingParenthesis(before:List[Char], after:List[Char],count:Int):List[Char] = (after,count) match {
    case (')'::_,0) => before
    case (')'::tail, count) => upToClosingParenthesis(before ++ ")", tail, count - 1)
    case ('('::tail, count) => upToClosingParenthesis(before ++ "(", tail, count + 1)
    case (head::tail, count) => upToClosingParenthesis(before ++ head.toString, tail, count)
  }

  def evalOperator(v:Long, s:List[Char]) = s match {
    case '+'::' '::exp => v + eval(exp)
    case '*'::' '::exp => v * eval(exp)
  }

  // Horrible hack to handle left to right evaluation 
  // - it relies on the fact that the numbers only have 1 digit
  def eval(s:String) : Long = 
    eval(s.reverse.replace('(','[').replace(')','(').replace('[',')').toList)

  def eval(s:List[Char]):Long = {
    s match {
      case ' '::tail => eval(tail)
      case '('::tail => {
        val e1 = upToClosingParenthesis(Nil, tail, 0)
        val v1 = eval(e1)
        val e2 = tail.slice(e1.length + 2, tail.length)
        assert(e2.length != 1)

        if (e1 == tail || e2 == Nil) eval(e1) else evalOperator(v1, e2)
      }
      case d::' '::'+'::' '::exp => d.toString.toInt + eval(exp)
      case d::' '::'*'::' '::exp => d.toString.toInt * eval(exp)
      case d::Nil => d.toString.toInt
    }
  }

  def fullEval1(strings:LazyList[String]) =
    fullInput.map(eval(_)).sum

  // part 2 - I probably should have used the parser combinators for both solutions !
  import scala.util.parsing.combinator._

  object Evaluator extends RegexParsers {
    def number: Parser[Long] = """\d+""".r ^^ { _.toLong }
    def term: Parser[Long] = number | "(" ~> expr <~ ")"
    def factor  : Parser[Long] = term ~ rep( "+" ~ term) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "+" ~ y) => x + y
      }
    }
    def expr  : Parser[Long] = factor ~ rep("*" ~ factor) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "*" ~ y) => x * y
      }
    }

    def apply(input: String): Long = parseAll(expr, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }

  def fullEval2(strings:LazyList[String]) =
    fullInput.map(Evaluator(_)).sum

  def main(args: Array[String]): Unit = {
    println("part1=" + fullEval1(fullInput))
    println("part2=" + fullEval2(fullInput))

  }
}