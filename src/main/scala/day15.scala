package advent_of_code

import scala.util._

@main def day15 =
  
  given Day = Day(15)
  
  val data = loadData

  part1
  part2

  case class State(numbers: Map[Long, Long] = Map.empty, last: Long, pos: Long) {
    def next: State =
      if !numbers.contains(last) 
        State(numbers + (last -> pos), 0L, pos + 1)
      else  
        State(numbers + (last -> pos), pos - numbers(last), pos + 1)
  }

  def find(num: Int): Long =
    val init = data.split(',').map(_.trim)
    val initSize = init.size
    val initNumbers = init.zipWithIndex.map((a,b) => (a.toLong, b.toLong + 1)).dropRight(1).toMap
    val initLast = init.last.toLong

    def seq(init: State) =
      def loop(state: State): LazyList[State] =
        val nextState = state.next
        nextState #:: loop(nextState)
      loop(init)

    seq(State(initNumbers, initLast, initSize))
      .drop(num - (initSize + 1))
      .take(1)
      .map(s => s.last).toList
      .head

  def part1 =
    given Part = PartA

    val result = find(2020)

    printResult(result)

  def part2 =
    given Part = PartB

    val result = find(30000000)

    printResult(result)