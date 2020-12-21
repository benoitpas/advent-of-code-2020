object Day21 {
  import scala.io.Source
  val fullInput = Source.fromResource("day21-input.txt").getLines.to(LazyList)

  val smallInput = LazyList(
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
    "trh fvjkl sbzzf mxmxvkd (contains dairy)",
    "sqjhc fvjkl (contains soy)",
    "sqjhc mxmxvkd sbzzf (contains fish)"
  )

  type Rule = (Set[String],Set[String])
  type Rules = LazyList[Rule]

  def parse(input: LazyList[String]) : Rules = 
    def split(s:String, sep:String) = s.split(sep).map(_.trim).toSet
    input.map(
      (s)=>s.split(" \\(contains ").toList match {
        case ingredients::allergens::_ => (split(ingredients, " "), split(allergens.dropRight(1), ","))
      })

  
  def combinationsWithOneAllergen(rules:Rules) = rules.combinations(2)
    .map( pair => (pair(0)._1.intersect(pair(1)._1), pair(0)._2.intersect(pair(1)._2)))
    .filter( twoRules => (twoRules._2.size == 1)).to(LazyList)

  def findOneRelation(rules:Rules) : (String, String) = 
    // First try to see if there are any 1-1 relationships
    val r = rules.filter(pair => pair._1.size == 1 && pair._2.size == 1).take(1).toList
    if (r.length == 1) (r.head._1.head, r.head._2.head) else
      // Try to combine 2 rules
      val combinations = combinationsWithOneAllergen(rules)
      val r = combinations.filter(pair => pair._1.size == 1).take(1)
      if (r.length == 1) (r.head._1.head,r.head._2.head) else
        // Reduce aggressively
        val allergens = rules.flatMap(_._2).toSet
        val m = allergens.map(allergen => (allergen, combinations.filter(_._2.head == allergen).map(_._1).reduce(_.intersect(_)))).filter(_._2.size == 1)
        (m.head._2.head, m.head._1)
    
     

  def noMoreAllegerns(rules:Rules) = rules.map(_._2).flatten.size == 0

  def remainingIngredient(rules:Rules) = rules.map(_._1).flatten

  def matchAllergens(rules:Rules) = {

    def loop(rules:Rules, matchedAllergens: Set[(String,String)]) : (LazyList[String], Set[(String,String)])= {
      if( noMoreAllegerns(rules))
        (remainingIngredient(rules), matchedAllergens)
      else
        val (ingredient, allergen) = findOneRelation(rules)
        val newRules = rules.map(pair => (pair._1 - ingredient, pair._2 - allergen))
        loop(newRules, matchedAllergens ++ Set((ingredient,allergen)) )

    }

    loop(rules, Set[(String,String)]())
  }

  def allergenIngredients(matchedAllergens:Set[(String,String)]) = matchedAllergens.toList.sortBy(_._2).map(_._1).mkString(",")

  def main(args: Array[String]): Unit = {
    val rules = parse(fullInput)
    val m = matchAllergens(rules)
    println("part1=" + m._1.length)
    println("part2=" + allergenIngredients(m._2))
  }
}