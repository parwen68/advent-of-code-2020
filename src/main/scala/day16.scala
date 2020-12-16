package advent_of_code

import scala.util._

@main def day16 =
  
  given Day = Day(16)
  
  val RulePattern = """([\s\w]+): (\d+)-(\d+) or (\d+)-(\d+)""".r
  
  case class Rule(name: String, intervals: Vector[Range])

  def parseRules(rules: String) = rules.split("\n").map { line => line match 
    case RulePattern(n, f1, t1, f2, t2) => Rule(n, Vector(f1.toInt to t1.toInt, f2.toInt to t2.toInt))
  }.toVector

  def parseTickets(tickets: String) = tickets.split("\n").drop(1).map { line =>
    line.split(',').map(_.toInt).toVector
  }.toVector

  val data = loadData

  val m = data.split("\n\n")

  val (rulesStr, ticketStr, otherStr) = (m(0), m(1), m(2))

  val rules = parseRules(rulesStr)
  val ticket = parseTickets(ticketStr).head
  val other = parseTickets(otherStr)

  extension (i: Int) def check(rule: Rule) =
    rule.intervals.exists(_ contains i)

  val checks = other.map(_.map(v => (v, rules.map(v.check(_)).forall(!_))))

  part1
  part2

  def part1 =
    given Part = PartA

    val result = checks.flatten.filter(_._2).foldLeft(0){(acc, ticket) => acc + ticket._1 }
    
    printResult(result)

  def part2 =
    given Part = PartB

    val validIdx = checks.zipWithIndex.filter((v, idx) => v.forall{ case (_,t) => !t } ).map(_._2)

    val fields = validIdx
      .map(idx => other(idx))
      .transpose
  
    def checkField(fieldValues: Vector[Int], rule: Rule) =
      if fieldValues.map(f => f.check(rule)).forall(identity)
        then Some(rule.name)
        else None

    def find(init: Map[Int, Set[String]]) =
      def loop(m: Map[Int, Set[String]], r: Map[Int, String]) : Map[Int, String]=
        if m.isEmpty
          then r
          else 
            val ones = m.filter((k,v) => v.size == 1)
            val values = ones.map((k,v) => v).flatten
            val nextM = m.map((k,v) => (k, v -- values)).filter((k,_) => !ones.keySet.contains(k)).toMap
            val nextR = r ++ ones.map((k,v) => (k,v.head)).toMap
            loop(nextM, nextR)

      loop(init, Map.empty)  

    val init = fields
      .map(f => rules.map(rule => checkField(f, rule)).flatten.toSet)
      .zipWithIndex
      .map((k,v) => (v,k))
      .toMap

    val result = find(init)
      .filter((k,v) => v.startsWith("departure"))
      .map((k,_) => k)
      .map(k => ticket(k).toLong)
      .product

    printResult(result)