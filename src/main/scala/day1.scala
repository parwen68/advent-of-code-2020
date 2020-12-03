package advent_of_code

@main def day1 = 
  given Day = Day(1)
  
  val data = loadData
  val numbers = data.split("\n").map(_.toInt).toList

  part1
  part2

  def part1 =
    given Part = PartA

    val result = for 
      n1 <- numbers
      n2 <- numbers
      sum = n1 + n2 if sum == 2020 
    yield n1 * n2

    printResult(result.take(1).head)

  def part2 =
    given Part = PartB

    val result = for 
      n1 <- numbers
      n2 <- numbers
      n3 <- numbers
      sum = n1 + n2 + n3 if sum == 2020 
    yield n1 * n2 * n3

    printResult(result.take(1).head)
