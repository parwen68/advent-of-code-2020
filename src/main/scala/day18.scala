package advent_of_code

import scala.util._

@main def day18 =
  
  given Day = Day(18)

  val data = loadData

  enum Token {
    case Plus
    case Mult
    case Open
    case Close
    case Num(v: Long)
  }
  import Token._
  def charToToken(c: Char): Token =
    c match 
      case '+' => Plus
      case '*' => Mult
      case '(' => Open
      case ')' => Close
      case d if d.isDigit => Num(d.toInt - 48)
      case _ => ??? 

  val rows = data.split('\n').map(v => v.filter(_ != ' ').map(c => charToToken(c)).toList).toList 
  
  extension (stack: List[Token]) def hasNoOp =
    stack.isEmpty || stack.head == Open

  def calc(tokens: List[Token]) =
    def loop(tokens: List[Token], stack: List[Long] = List()): Long = 
      tokens match 
        case Nil => stack(0)
        case Num(v) :: tail => loop(tail, v :: stack)
        case Plus :: tail => 
          loop(tail, (stack(0) + stack(1)) :: stack.drop(2))
        case Mult :: tail => 
          loop(tail, (stack(0) * stack(1)) :: stack.drop(2))
        case _ => ???
    loop(tokens) 

  part1    
  part2

  def part1 =
    given Part = PartA      

    def parse(tokens: List[Token]) =
      def loop(tokens: List[Token], opStack: List[Token], r: List[Token]): List[Token] =
        tokens match {
          case Nil => r ++ opStack
          case (n: Num) :: tail => loop(tail, opStack, r :+ n)
          case (op @ Mult) :: tail => 
            if opStack.hasNoOp 
              then loop(tail, op :: opStack, r)
              else loop(tail, op :: opStack.drop(1), r :+ opStack.head)
          case (op @ Plus) :: tail => 
            if opStack.hasNoOp 
              then loop(tail, op :: opStack, r)
              else loop(tail, op :: opStack.drop(1), r :+ opStack.head)
          case (op @ Open) :: tail => loop(tail, op :: opStack, r)
          case (op @ Close) :: tail => 
            loop(tail, opStack.dropWhile(_ != Open).drop(1),  r ++ opStack.takeWhile(_ != Open))
        }
      loop(tokens, List(), List())

    val result = rows.map(parse).map(calc).sum

    printResult(result)

  def part2 =
    given Part = PartB 

    def parse(tokens: List[Token]) =
      def loop(tokens: List[Token], opStack: List[Token], r: List[Token]): List[Token] =
        tokens match {
          case Nil => r ++ opStack
          case (n: Num) :: tail => loop(tail, opStack, r :+ n)
          case (op @ Mult) :: tail => 
            if opStack.hasNoOp 
              then loop(tail, op :: opStack, r)
              else loop(tail, op :: opStack.dropWhile(_ == Plus), r ++ opStack.takeWhile(_ == Plus))
          case (op @ Plus) :: tail => 
              loop(tail, op :: opStack, r)
          case (op @ Open) :: tail => loop(tail, op :: opStack, r)
          case (op @ Close) :: tail => 
            loop(tail, opStack.dropWhile(_ != Open).drop(1),  r ++ opStack.takeWhile(_ != Open))
        }
      loop(tokens, List(), List())

    val result = rows.map(parse).map(calc).sum

    printResult(result)

