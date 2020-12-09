package advent_of_code

@main def day9 =
  
  given Day = Day(9)
  
  val data = loadData

  val numbers = data.split("\n").map(_.toLong).toList

  def findNumber(sequenceLength: Int) = 
    val initialNums: List[Long] = numbers.take(sequenceLength)
    val initialNumsWithSums: List[(Long, List[Long])] = initialNums.map(n1 => 
      (n1 -> initialNums.filter(_ != n1).map(n2 => n1 + n2).toList)
    )

    case class Context(sums: List[(Long, List[Long])], remaining: List[Long])
    
    def iterate: LazyList[Context] = 
      def loop(ctx: Context): LazyList[Context] = 
        val next = ctx.remaining.head
        if !ctx.sums.flatMap((_, v) => v).contains(next)
          then ctx #:: LazyList.empty
          else 
            val sumsWithOldestDroped = ctx.sums.drop(1)
            val newSums = sumsWithOldestDroped :+ (next, sumsWithOldestDroped.map(_._1 + next).toList) 
            val newRemaining = ctx.remaining.drop(1)
            Context(newSums, newRemaining) #:: loop(Context(newSums, newRemaining))

      loop(Context(initialNumsWithSums, numbers.drop(sequenceLength)))
    
    iterate.last.remaining.head

  part1
  part2

  def part1 =
  
    given Part = PartA

    val number = findNumber(25)

    printResult(number)

  def part2 =
    given Part = PartB

    val number = findNumber(25)
    
    def iterate: LazyList[List[Long]] = 
      def loop(n: List[Long]): LazyList[List[Long]] = 
        val candidateList = infiniteList(1).map(v => n.take(v.toInt)).takeWhile(_.sum <= number).last
        candidateList #:: loop(n.drop(1))

      loop(numbers)

    val result = iterate.dropWhile(_.sum != number).head

    printResult(result.max + result.min)