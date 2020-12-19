object Day19 {
  import scala.io.Source
  val fullInput = Source.fromResource("day19-input.txt").getLines.to(LazyList)

  val smallInput = LazyList(
    "0: 4 1 5",
    "1: 2 3 | 3 2",
    "2: 4 4 | 5 5",
    "3: 4 5 | 5 4",
    "4: \"a\"",
    "5: \"b\"",
    "",
    "ababbb",
    "bababa",
    "abbbab",
    "aaabbb",
    "aaaabbb"
  )

  case class LetterRule(index:Int, letter:Char)

  object LetterRule {
    val letterRuleRegex = "^(\\d+):\\s+\"(\\w)\"$".r

    def apply(index:Int, letter:Char): LetterRule = LetterRule(index, letter)

    def unapply(s:String):Option[(Int,Char)] =
      letterRuleRegex.findFirstMatchIn(s).flatMap( (m) =>
          Some(m.group(1).toInt -> m.group(2)(0)))
    }

  case class SequenceRule(index:Int, rules: LazyList[List[Int]]) 
  
  import scala.util.matching.Regex.Match
  def groupToList(m: Match, l: List[Int]) = {
    def groupToInt(index: Int) = 
      if (m.group(index) == "") List() else  List(m.group(index).toInt)
    l.flatMap(groupToInt)
  }

  object SequenceRule {
    val sequenceRuleRegex = "^(\\d+):\\s+(\\d+)\\s*(\\d*)\\s*(\\d*)$".r

    def apply(index:Int, rules: LazyList[List[Int]]): SequenceRule = SequenceRule(index, rules)

    def unapply(s:String): Option[(Int, LazyList[List[Int]])] = 
      sequenceRuleRegex.findFirstMatchIn(s).flatMap( (m) =>
          Some((m.group(1).toInt -> LazyList(groupToList(m, List(2,3,4))))))
  }

  case class OrRule(index:Int, rules: LazyList[List[Int]]) 
  object OrRule {
    val sequenceRuleRegex = "^(\\d+):\\s+(\\d+)\\s*(\\d*)\\s*\\|\\s*(\\d*)\\s*(\\d*)$".r

    def apply(index:Int, rules: LazyList[List[Int]]): SequenceRule = SequenceRule(index, rules)

    def unapply(s:String): Option[(Int, LazyList[List[Int]])] = 
      sequenceRuleRegex.findFirstMatchIn(s).flatMap( (m) =>
          Some((m.group(1).toInt -> LazyList(groupToList(m, List(2,3)),groupToList(m, List(4,5))))))
  }

  enum Rule {
    case Depends(rules: LazyList[List[Int]]) 
    case Matches(letter: Char)
  }

  def countValidMessages(input : LazyList[String]) = {
    val sections = Day16.extractSections(input)
    val rules = sections(0).foldLeft(Map[Int,Rule]())((map, rule) => {
      map ++ List(rule match {
      case OrRule(index, rules) => (index, Rule.Depends(rules))
      case SequenceRule(index, rules) => (index, Rule.Depends(rules))
      case LetterRule(index, letter) => (index, Rule.Matches(letter))
    })})

    def checkSeqRules(s: List[Char], rules:List[Int]): (Boolean, List[Char]) =
      rules.foldLeft((true,s))(
        (acc,rule) => acc match {
          case((false,_)) => acc
          case((true,head::tail)) => checkRule(head::tail, rule)
          case((true,_)) => (false,Nil)
        })
 
    def checkRule(s: List[Char], ruleIndex: Int): (Boolean, List[Char]) = (s, rules(ruleIndex)) match {
      case (head::tail, Rule.Matches(letter))  => if (head == letter) (true, tail) else (false, s)
      case (head::tail, Rule.Depends(moreRules)) => moreRules.foldLeft((false, s))(
        (acc,seqRules) => if (acc._1 ) acc else checkSeqRules(s, seqRules))
      case (Nil,_) => (true, s)
    }

    //Check messages
    sections(1).map((s) => checkRule(s.toList,0)).filter((pair) => pair._1 && pair._2.length == 0).size
  }

  def main(args: Array[String]): Unit = {
    println("part1=" + countValidMessages(fullInput))
  }
}