object Day19 {
  import scala.io.Source
  val fullInput = Source.fromResource("day19-input.txt").getLines.to(LazyList)

  val smallInput1 = LazyList(
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

  val smallInput2 = LazyList(
    "42: 9 14 | 10 1",
    "9: 14 27 | 1 26",
    "10: 23 14 | 28 1",
    "1: \"a\"",
    "11: 42 31",
    "5: 1 14 | 15 1",
    "19: 14 1 | 14 14",
    "12: 24 14 | 19 1",
    "16: 15 1 | 14 14",
    "31: 14 17 | 1 13",
    "6: 14 14 | 1 14",
    "2: 1 24 | 14 4",
    "0: 8 11",
    "13: 14 3 | 1 12",
    "15: 1 | 14",
    "17: 14 2 | 1 7",
    "23: 25 1 | 22 14",
    "28: 16 1",
    "4: 1 1",
    "20: 14 14 | 1 15",
    "3: 5 14 | 16 1",
    "27: 1 6 | 14 18",
    "14: \"b\"",
    "21: 14 1 | 1 14",
    "25: 1 1 | 1 14",
    "22: 14 14",
    "8: 42",
    "26: 14 22 | 1 20",
    "18: 15 15",
    "7: 14 5 | 1 21",
    "24: 14 1",
    "",
    "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
    "bbabbbbaabaabba",
    "babbbbaabbbbbabbbbbbaabaaabaaa",
    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
    "bbbbbbbaaaabbbbaaabbabaaa",
    "bbbababbbbaaaaaaaabbababaaababaabab",
    "ababaaaaaabaaab",
    "ababaaaaabbbaba",
    "baabbaaaabbaaaababbaababb",
    "abbbbabbbbaaaababbbbbbaaaababb",
    "aaaaabbaabaaaaababaa",
    "aaaabbaaaabbaaa",
    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
    "babaaabbbaaabaababbaabababaaab",
    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba",
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

  def extractRules(input : LazyList[String]) = input.foldLeft(Map[Int,Rule]())(
    (map, rule) =>
      map ++ List(rule match
        case OrRule(index, rules) => (index, Rule.Depends(rules))
        case SequenceRule(index, rules) => (index, Rule.Depends(rules))
        case LetterRule(index, letter) => (index, Rule.Matches(letter))
    ))
 
  def countValidMessages1(input : LazyList[String]) = {
    val sections = Day16.extractSections(input)
    val rules = extractRules(sections(0))

    def checkRule(s: List[Char], ruleIndex: Int): (Boolean, List[Char]) = (s, rules(ruleIndex)) match {
      case (head::tail, Rule.Matches(letter))  => if (head == letter) (true, tail) else (false, s)
      case (head::tail, Rule.Depends(moreRules)) => moreRules.foldLeft((false, s))(
        (acc,seqRules) => if (acc._1 ) acc else checkSeqRules(s, seqRules))
      case (Nil,_) => (true, s)
    }
  
    def checkSeqRules(s: List[Char], rules:List[Int]): (Boolean, List[Char]) =
      rules.foldLeft((true,s))(
        (acc,rule) => acc match {
          case((false,_)) => acc
          case((true,head::tail)) => checkRule(head::tail, rule)
          case((true,_)) => (false,Nil)
        })
  
    //Check messages
    sections(1).map((s) => checkRule(s.toList,0)).filter(
      (pair) => pair._1 && pair._2.length == 0).size
  }

  // Regex hint from reddit..that would also have worked for part 1
  def countValidMessages2(input : LazyList[String]) = {
    val sections = Day16.extractSections(input)
    val originalRules = extractRules(sections(0))
    val rules = originalRules ++ Map(
      8 -> Rule.Depends(LazyList(List(42), List(42,  8))),
      11 -> Rule.Depends(LazyList(List(42, 31), List(42, 11, 31))))

    def generateRegex(rule:Int, allRules:Map[Int, Rule], depth:Int) :String = 
      if (depth > 0)
        allRules(rule) match {
          case Rule.Matches(l: Char) => l.toString()
          case Rule.Depends(seq:LazyList[List[Int]]) => {
            "(" + seq.map((rules) => rules.flatMap(
              (rule) => generateRegex(rule, allRules, depth - 1)).mkString
              ).mkString("|")+")"
          }
        }
      else ""

    val regexString = generateRegex(0, rules, 14)
    import scala.util.matching.Regex
    val regex = Regex(regexString)
    sections(1).map((s) => if (regex.matches(s)) 1 else 0).sum
  }

  def main(args: Array[String]): Unit = {
    println("part1=" + countValidMessages1(fullInput))
    println("part2=" + countValidMessages2(fullInput))
  }
}