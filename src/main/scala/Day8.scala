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

    def run(instructions:Array[String]) = {

      val pInstructions = parse(instructions)
      import scala.annotation.tailrec
      @tailrec
      def loop(pc:Int, acc:Int, previousPc:Set[Int]) : Int =
        if (!previousPc.contains(pc)) {
          pInstructions(pc) match {
            case ("acc",inc) => loop(pc+1, acc+inc, previousPc + pc)
            case ("jmp",inc) => loop(pc+inc, acc, previousPc + pc)
            case ("nop",_) => loop(pc+1, acc, previousPc + pc)
          }
        } else acc

      loop(0, 0, Set())
    }

    def main(args: Array[String]): Unit = {
      println("part1="+run(allInstructions))
    }
}