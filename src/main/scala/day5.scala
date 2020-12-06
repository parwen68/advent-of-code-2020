package advent_of_code

@main def day5 =
  
  given Day = Day(5)
  
  val data = loadData

  part1
  part2

  def keepLower(r: Range): Range =
    Range(r.start, r.end - (r.size + 1) / 2)

  def keepUpper(r: Range): Range =
    Range(r.start + (r.size + 1) / 2, r.end)

  import scala.annotation.tailrec  
  def findRow(ticket: List[Char]) =
    @tailrec def next(s: List[Char], r: Range): Range =
      s match
        case 'F' :: tail => next(tail, keepLower(r))
        case 'B' :: tail => next(tail, keepUpper(r))
        case _ => r

    next(ticket, 0 to 127).start

  def findColumn(ticket: List[Char]) = 
    @tailrec def next(s: List[Char], r: Range): Range =
      s match
        case 'L' :: tail => next(tail, keepLower(r))
        case 'R' :: tail => next(tail, keepUpper(r))
        case _ => r
   
    next(ticket takeRight 3, 0 to 7).start

  extension (tickets: Array[String]) def getSeats =
    tickets
      .map(_.toList)
      .map(ticket => (findRow(ticket), findColumn(ticket)))
      .map((row, column) => row * 8 + column) 

  def part1 =
    given Part = PartA

    val tickets = data.split("\n")
    val max = tickets.getSeats.max

    printResult(max)

  def part2 =
    given Part = PartB

    val tickets = data.split("\n")

    val seats = tickets.getSeats.toSet
    val allSeats = (seats.min to seats.max).toSet
    
    printResult(allSeats.diff(seats).head)