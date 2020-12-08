object Day8 {

  import scala.io.Source
  val allInstructions = Source.fromResource("day8-input.txt").getLines.toArray

  val instructions1 = Array(
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6")

  def parse(instructions:Array[String]) = instructions.map(_.split(" ").toList match {
    case instruction::operand::Nil => (instruction, operand.toInt)
  })

  def run(instructions:Array[(String,Int)]): (Int,Int) = {

    import scala.annotation.tailrec
    @tailrec
    def loop(pc:Int, acc:Int, previousPc:Set[Int]) : (Int,Int) =
      if (!previousPc.contains(pc) && pc < instructions.length) {
        instructions(pc) match {
          case ("acc",inc) => loop(pc+1, acc+inc, previousPc + pc)
          case ("jmp",inc) => loop(pc+inc, acc, previousPc + pc)
          case ("nop",_) => loop(pc+1, acc, previousPc + pc)
         }
      } else (pc,acc)

    loop(0, 0, Set())
  }

  def run(instructions:Array[String]): (Int,Int) = run(parse(instructions))


  def patch(instructions:Array[String]) = {
    val pInstructions = parse(instructions)

    (pInstructions.length -1 to 0 by -1).to(LazyList).flatMap( (i) => {
      val mInstructions = pInstructions.clone
      mInstructions(i) match {
        case ("nop", op) => {
          mInstructions(i) = ("jmp",op)
          List(run(mInstructions))
          }
        case ("jmp", op) => {
          mInstructions(i) = ("nop",op)
          List(run(mInstructions))
          }
        case _ => List()
      }

    }).filter(_._1 == pInstructions.length).take(1).toList
  }

  def main(args: Array[String]): Unit = {
    println("part1="+run(allInstructions)._2)
    println("part2="+patch(allInstructions)(0)._2)
  }
}