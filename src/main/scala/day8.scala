package advent_of_code

@main def day8 =
  
  given Day = Day(8)
  
  val data = loadData

  val Pattern = """(\w{3}) ([+-]{1}\d+)""".r

  type Program = Map[Int, Instruction]
  
  case class Instruction(inst: String, param: Int, execCnt:Int = 0)
  case class Cpu(
    ptr: Int = 0, 
    acc: Int = 0, 
    program: Program, 
    looped: Boolean = false,
    terminated: Boolean = false
  )
  
  val program: Program = data.split("\n")
    .zipWithIndex
    .map { case (Pattern(a,b), idx) => (idx -> Instruction(a, b.toInt)) }
    .toMap

  def execute(cpu: Cpu): Cpu = 
    if cpu.ptr > program.keys.max
      then cpu.copy(terminated = true) 
      else
        cpu.program(cpu.ptr) match {
          case i @ Instruction(_, _, c) if c > 0 => cpu.copy(looped = true)
          case i @ Instruction("nop", v, c) => Cpu(cpu.ptr + 1, cpu.acc, cpu.program + (cpu.ptr -> i.copy(execCnt = i.execCnt + 1)), c > 0)
          case i @ Instruction("acc", v, c) => Cpu(cpu.ptr + 1, cpu.acc + v, cpu.program + (cpu.ptr -> i.copy(execCnt = i.execCnt + 1)), c > 0)
          case i @ Instruction("jmp", v, c) => Cpu(cpu.ptr + v, cpu.acc, cpu.program + (cpu.ptr -> i.copy(execCnt = i.execCnt + 1)), c > 0)
    }

  def execution(cpu: Cpu): LazyList[Cpu] = 
    LazyList.cons(cpu, execution(execute(cpu)))

  part1
  part2

  def part1 =
    given Part = PartA

    val result = execution(Cpu(program = program)).dropWhile(!_.looped).head
    println(result.acc)

  def part2 =
    given Part = PartB

    val nops = program.collect{ case (pos, Instruction("nop", _, _)) => pos } 
    val jmps = program.collect{ case (pos, Instruction("jmp", _, _)) => pos } 

    val modified = 
      nops.map(idx => program + (idx -> program(idx).copy(inst = "jmp"))) ++
      jmps.map(idx => program + (idx -> program(idx).copy(inst = "nop")))

    val r = modified.map { program => 
      execution(Cpu(program = program)).dropWhile(cpu => !cpu.looped && !cpu.terminated).head
    }

    val result = r.dropWhile(!_.terminated).head

    printResult(result.acc)