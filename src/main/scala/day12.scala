package advent_of_code

@main def day12 =
  
  given Day = Day(12)
  
  val data = loadData

  val Pattern = """(\w)(\d+)""".r

  enum Instruction {
    case MoveNorth(steps: Int)
    case MoveSouth(steps: Int)
    case MoveEast(steps: Int)
    case MoveWest(steps: Int)
    case MoveForward(steps: Int)
    case TurnLeft(degrees: Int)
    case TurnRight(degrees: Int)
  }
  import Instruction._

  case class Pos(x: Int = 0, y: Int = 0, d: Int = 90)

  def parse(s: String): Instruction = s match {
    case Pattern(action, value) => action match {
      case "N" => MoveNorth(value.toInt)
      case "S" => MoveSouth(value.toInt)
      case "E" => MoveEast(value.toInt)
      case "W" => MoveWest(value.toInt)
      case "L" => TurnLeft(value.toInt)
      case "R" => TurnRight(value.toInt)
      case "F" => MoveForward(value.toInt)
    }
  }

  val instructions = data.split("\n").map(parse)

  part1
  part2

  def part1 =
    given Part = PartA

    def turn(from: Int, d: Int) = {
      val n = from + d
      (if n < 0 then (n + 360) else n) % 360
    }

    val pos = instructions.foldLeft(Pos()){ (p, instr) => instr match
      case MoveNorth(steps) => p.copy(y = p.y + steps)
      case MoveSouth(steps) => p.copy(y = p.y - steps)
      case MoveEast(steps) => p.copy(x = p.x + steps)
      case MoveWest(steps) => p.copy(x = p.x - steps)
      case TurnLeft(degrees) => p.copy(d = turn(p.d, -degrees))
      case TurnRight(degrees) => p.copy(d = turn(p.d, degrees))
      case MoveForward(steps) => { p.d match {
        case   0 => p.copy(y = p.y + steps)
        case  90 => p.copy(x = p.x + steps)
        case 180 => p.copy(y = p.y - steps)
        case 270 => p.copy(x = p.x - steps)
      }}
    }

    val result = Math.abs(pos.x) + Math.abs(pos.y)

    printResult(result)
    
  def part2 =
    given Part = PartB

    def turn(wp: Pos, d: Int) = {
      d match {
        case ( 90 | -270) => wp.copy(x = wp.y, y = -wp.x)
        case (180 | -180) => wp.copy(x = -wp.x, y = -wp.y)
        case (-90 |  270)  => wp.copy(x = -wp.y, y = wp.x)
      }
    }

    val (pos, _) = instructions.foldLeft((Pos(), Pos(10,1))){ 
      case ((ship, wp), instr) => instr match
        case MoveNorth(steps) => (ship, wp.copy(y = wp.y + steps))
        case MoveSouth(steps) => (ship, wp.copy(y = wp.y - steps))
        case MoveEast(steps) => (ship, wp.copy(x = wp.x + steps))
        case MoveWest(steps) => (ship, wp.copy(x = wp.x - steps))
        case TurnLeft(degrees) => (ship, turn(wp, -degrees))
        case TurnRight(degrees) => (ship, turn(wp, degrees))
        case MoveForward(steps) => (
          ship.copy(x = ship.x + steps * wp.x, y = ship.y + steps * wp.y), wp)
    }

    val result = Math.abs(pos.x) + Math.abs(pos.y)

    printResult(result)
