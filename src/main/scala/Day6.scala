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

  def processOneForm(form:String) = form.replaceAll("\\s","").toSet.size

  def processAllForms(forms:Seq[String]) =
    Day4.extractBlocks(forms).map(processOneForm).sum

  def main(args: Array[String]): Unit = {
    println("part1=" + processAllForms(fullInput))
  }
}