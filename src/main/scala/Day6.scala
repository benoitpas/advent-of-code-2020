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

  def processOneForm1(form:String) = form.replaceAll("\\s","").toSet.size

  def processOneForm2(form:String) = form.split(" ").map(_.toSet).reduce(_.intersect(_)).size

  def processAllForms(forms:Seq[String], processOneForm : (String => Int)) =
    Day4.extractBlocks(forms).map(processOneForm).sum

  def processAllForms1(forms:Seq[String]) = processAllForms(forms, processOneForm1)
  def processAllForms2(forms:Seq[String]) = processAllForms(forms, processOneForm2)
  
  def main(args: Array[String]): Unit = {
    println("part1=" + processAllForms1(fullInput))
    println("part2=" + processAllForms2(fullInput))
  }
}