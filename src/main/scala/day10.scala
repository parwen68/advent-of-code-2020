package advent_of_code

import collection.immutable._
val IntOrdering = implicitly[Ordering[Int]]

@main def day10 =
  
  given Day = Day(10)
  
  val data = loadData

  val adapters = data.split("\n").map(_.toInt).sorted.toList
  val max = adapters.max + 3

  val allAdapters = adapters :+ max
  
  part1
  part2

  def part1 = 
    given Part = PartA

    val diffs = (0 +: allAdapters).zip(allAdapters).map((n1, n2) => n2 - n1)

    val a = diffs.groupBy(identity).map((k,v) => (k,v.size))

    val result = a(1) * a(3)
    
    printResult(result)

  def part2 =
    given Part = PartB

    val adj = (0 +: allAdapters).map {
      v => ( v -> List(1, 2, 3).map(v + _)
        .filter(allAdapters.contains(_))
        .toList
      )
    }

    val reverseOrderedAdj = SortedMap.empty(IntOrdering.reverse) ++ adj 

    val pathCounts = reverseOrderedAdj.foldLeft(Map[Int,Long]()){ 
      case (pathCounts, (key, ancestors)) => 
        pathCounts + (key -> {
          val sum = ancestors.map(ancestor => pathCounts(ancestor)).sum 
          if sum == 0 then 1 else sum
      })
    }

    val result = pathCounts(0)

    printResult(result)


