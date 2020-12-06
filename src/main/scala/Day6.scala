object Day6 {

  import scala.io.Source
  val fullInput = Source.fromResource("day6-input.txt").getLines.to(LazyList)

  val smallInput = 
    """abc

a
b
c

ab
ac

a
a
a
a

b
""".split("\n").toIndexedSeq

  // The operation is parametrised
  def processOneForm[T](op: ((Set[Char],Set[Char]) => Set[Char]))(form:String)
    = form.split(" ").map(_.toSet).toList.reduce(op).size

  def processAllForms(forms:Seq[String], processOneForm : (String => Int)) =
    Day4.extractBlocks(forms).map(processOneForm).sum

  def processAllForms1(forms:Seq[String]) = processAllForms(forms, 
    processOneForm((s1:Set[Char],s2:Set[Char]) => s1.union(s2)))

  def processAllForms2(forms:Seq[String]) = processAllForms(forms, 
    processOneForm((s1:Set[Char],s2:Set[Char]) => s1.intersect(s2)))

  def main(args: Array[String]): Unit = {
    println("part1=" + processAllForms1(fullInput))
    println("part2=" + processAllForms2(fullInput))
  }
}