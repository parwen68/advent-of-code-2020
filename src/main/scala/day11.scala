package advent_of_code

@main def day11 =
  
  given Day = Day(11)
  
  val data = loadData

  val room = data.split("\n")
  val width = room(0).size
  val height = room.size

  val coords = 
    for y <- -1 to 1; x <- -1 to 1; if !(x == 0 && y == 0) 
    yield (y,x)

  part1
  part2

  type Room = Array[String]

  def part1 =
    given Part = PartA

    def occupied(room: Room, y: Int, x: Int) = 
      coords.map { (dy,dx) => 
        val (yy, xx) = (y + dy, x + dx) 
        if xx >= 0 && xx < width && yy >= 0 && yy < height
          then if room(yy)(xx) == '#' then 1 else 0
          else 0
      }.sum

    def gen(room: Room) =
      room.zipWithIndex.map { (row, y) => 
        row.zipWithIndex.map { (seat, x) =>
          val occ = occupied(room, y, x)
          room(y)(x) match 
            case 'L' if occ == 0 => '#'
            case '#' if occ >= 4 => 'L'
            case r => r
        }.mkString
      }

    def generations(room: Room): LazyList[Room] =
      def loop(room: Room): LazyList[Room] =
        val nextRoom = gen(room) 
        nextRoom #:: loop(nextRoom)

      loop(room)

    val stable = generations(room)
      .zip(generations(room).drop(1))
      .dropWhile((a,b) => a.toSeq != b.toSeq)
      .head

    val result = stable._1.map(s => s.count(_ == '#')).sum
    printResult(result)

  def part2 =
    given Part = PartB

    def stream(room: Room, y: Int, x: Int, dy: Int, dx: Int) =
      def loop(yy: Int, xx: Int): LazyList[Char] =
        if (xx >= 0 && xx < width && yy >= 0 && yy < height) 
          val status = room(yy)(xx)
          if status != '.' 
            then status #:: LazyList.empty
            else status #:: loop(yy + dy, xx + dx)
        else LazyList.empty
      loop(y + dy,x + dx)

    def occupied(room: Room, y: Int, x: Int): Int = 
      coords.map { case (yy,xx) => 
        stream(room, y, x, yy, xx).lastOption.getOrElse('.')
      }.filter(_ == '#').size

    def gen(room: Room) =
      room.zipWithIndex.map { (row, y) => 
        row.zipWithIndex.map { (seat, x) =>
          val occ = occupied(room, y, x)
          room(y)(x) match 
            case 'L' if occ == 0 => '#'
            case '#' if occ >= 5 => 'L'
            case r => r
        }.mkString
      }

    def generations(room: Room): LazyList[Room] =
      def loop(room: Room): LazyList[Room] =
        val nextRoom = gen(room) 
        nextRoom #:: loop(nextRoom)

      loop(room)

    val stable = generations(room)
      .zip(generations(room).drop(1))
      .dropWhile((a,b) => a.toSeq != b.toSeq)
      .head

    val result = stable._1.map(s => s.count(_ == '#')).sum
    printResult(result)
    