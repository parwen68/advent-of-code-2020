package advent_of_code

@main def day3 = 
  given Day = Day(3)
  
  val data = loadData
  val world = data.split("\n").toVector.map(_.toVector)

  val worldWidth = world.map(_.size).head
  val worldHeight = world.size

  val startCoordinate = (0,0)
  
  part1
  part2

  def part1 =
    given Part = PartA

    def next(coord: (Int, Int)): (Int, Int) =
      (coord._1 + 1, coord._2 + 3)

    val coordinates = infiniteList()
      .scanLeft(startCoordinate)((coordinate, _) => next(coordinate))
      .takeWhile((down,_) => down < worldHeight)

    val result = coordinates
      .map((down,right) => world(down)(right % worldWidth))
      .count(_ == '#')

    printResult(result)

  def part2 =
    given Part = PartB
    
    def next(right: Int, down: Int) = 
      (coord: (Int, Int)) => (coord._1 + down, coord._2 + right) 

    val slopes = List((1,1), (3,1), (5, 1), (7, 1), (1, 2))

    val counts = for
      slope <- slopes
      coordinates = infiniteList()
        .scanLeft(startCoordinate)((coordinate, _) => next(slope._1,slope._2)(coordinate))
        .takeWhile((down,_) => down < worldHeight)
      count = coordinates.map((down,right) => world(down)(right % worldWidth)).count(_ == '#')
    yield count

    printResult(counts.map(_.toLong).product)