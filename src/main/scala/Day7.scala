object Day7 {

  import scala.io.Source
  val allRules = Source.fromResource("day7-input.txt").getLines.to(LazyList)


  val rules1 = List(
    "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags.")
 
  val rules2 = List(
    "shiny gold bags contain 2 dark red bags.",
    "dark red bags contain 2 dark orange bags.",
    "dark orange bags contain 2 dark yellow bags.",
    "dark yellow bags contain 2 dark green bags.",
    "dark green bags contain 2 dark blue bags.",
    "dark blue bags contain 2 dark violet bags.",
    "dark violet bags contain no other bags.")
 
    type BagType = (String,String)
  type Rules = Map[BagType,List[(Int, BagType)]]

  // No error handling, will break if incorrect input 
  def parseBag(bag: String): BagType = bag.split(" ").toList match {
    case a1::a2::_::Nil => (a1,a2)
  }

  def bagInstanceRegex = "(\\d+)\\s+(\\w+)\\s+(\\w+)".r
  def parseBagList(bagList: String) : List[(Int, BagType)]= bagList.split(",").toList.map { 
    (s) => bagInstanceRegex.findFirstMatchIn(s) match {
      case Some(m) if (m.groupCount == 3) => (m.group(1).toInt, (m.group(2),m.group(3)))
      case None => (0,("","")) // no other bags case
    }

  }

  def parseRule(rule:String) = rule.split("contain").toList match {
    case obj::subj::_ => (parseBag(obj),parseBagList(subj))
  }
  def parseRules(rules:Seq[String]) = rules.map(parseRule)

  def findContain(bag:BagType, rules: Rules) :Set[BagType] = 
    rules.toSet.flatMap( (kv) => {
        val bags :Set[BagType] = kv._2.map((i,l) => l).toSet;
        if (bags.contains(bag)) Set(kv._1) ++ findContain(kv._1,rules)
          else Set[BagType]()
    })

  val rules1Map = parseRules(rules1).toMap
  val rules2Map = parseRules(rules2).toMap
  val allRulesMap = parseRules(allRules).toMap

  def nbBagContain(bagType:BagType, rules:Rules)  = findContain(bagType, rules).size

  def containCount(bagType:BagType, rules:Rules) : Int = 
    rules(bagType).map { (bagInstance) => bagInstance._1 *
      (if(bagInstance._1 > 0 ) 1 + containCount(bagInstance._2, rules) else 0)
      }.sum

  val bt = ("shiny","gold")

  def main(args: Array[String]): Unit = {
    println("part1="+nbBagContain(bt,allRulesMap))
    println("part2="+containCount(bt,allRulesMap))
  }
}