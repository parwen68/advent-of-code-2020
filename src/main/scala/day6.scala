package advent_of_code

@main def day6 =
  
  given Day = Day(6)
  
  val data = loadData

  part1
  part2

  def part1 =
    given Part = PartA

    val result = data
      .split("\n\n")
      .map(_.replace("\n", "").toSet.size).sum

    printResult(result)  

  def part2 =
    given Part = PartB

    val result = data
      .split("\n\n")
      .map(_.split("\n").map(_.toSet).reduce(_ intersect _).size )
      .sum

    printResult(result)  