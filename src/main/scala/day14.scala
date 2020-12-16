package advent_of_code

import scala.util._

@main def day14 =
  
  given Day = Day(14)
  
  val data = loadData

  val MaskPattern = """mask = ([X01]{36})""".r
  val MemPattern  = """mem\[(\d+)\] = (\d+)""".r

  case class Mask(value: String)
  case class Mem(pos: Long, value: String)

  def valToBinary(v: String) = v.toInt.toBinaryString.reverse.padTo(36, '0').reverse 

  val parsed = data.split('\n').map{ _ match 
    case MaskPattern(mask) => Mask(mask)
    case MemPattern(pos, value) => Mem(pos.toLong, valToBinary(value))  
  }

  val allZero = "0".repeat(36)
  val initMem = Map[Long, String]().withDefaultValue(allZero)
  val initMask = Mask(allZero)

  part1
  part2

  def part1 =
    given Part = PartA

    case class Cpu(mem: Map[Long, String] = initMem, mask: Mask = initMask) {
      def exec(inst: Mem) =
        val Mem(pos, value) = inst
        val newMem = mem + (pos -> value.zip(mask.value).map { 
          case (v,'X') => v
          case (_, m) => m
        }.mkString )
        this.copy(mem = newMem)
      
      def sum = 
        mem.values.map(b => java.lang.Long.parseLong(b, 2)).sum
    }

    val result = parsed.foldLeft(Cpu()){ (cpu, i) => i match
      case mask: Mask => cpu.copy(mask = mask)
      case mem: Mem => cpu.exec(mem)
    }.sum

    printResult(result)

  def part2 =
    given Part = PartB

    case class Cpu(mem: Map[Long, String] = initMem, mask: Mask = initMask) {
      def exec(inst: Mem) =
        val Mem(pos, value) = inst
        val address = pos.toBinaryString.reverse.padTo(36, '0').reverse
        val newAddress = address.zip(mask.value).map{ 
          case (v,'0') => v
          case (_,'1') => '1'
          case (_,'X') => 'X'
        }.mkString

        val poss = newAddress.zipWithIndex.filter{ (a,i) => a == 'X'}.map(_._2)
        val adresses = poss.toSet.subsets.toList
          .map(set => set.foldLeft(newAddress)((a, i) => a.updated(i, '0')))
          .map(_.replaceAll("X", "1"))
          .map(s => java.lang.Long.parseLong(s, 2))

        val newMem = adresses.foldLeft(mem){ (m, pos) => m + (pos -> value) }

        this.copy(mem = newMem)
      
      def sum = 
        mem.values.map(b => java.lang.Long.parseLong(b, 2)).sum
    }

    val result = parsed.foldLeft(Cpu()){ (cpu, i) => i match
      case mask: Mask => cpu.copy(mask = mask)
      case mem: Mem => cpu.exec(mem)
    }.sum

    printResult(result)
    
