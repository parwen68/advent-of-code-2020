package advent_of_code

@main def day7 =
  
  given Day = Day(7)
  
  val data = loadData

  val keyPattern = """([\w\s]+) bags contain""".r
  val listPattern = """(\d+) ([\w\s]+) bag[s]{0,1}""".r

  val adj = data.split("\n")
    .map(line => (
      keyPattern.findFirstMatchIn(line).map(_.group(1)).toList(0),  
      listPattern.findAllMatchIn(line).map(m => (m.group(2) -> m.group(1))).toMap
    )).toMap

  def find(start: String, visited: Set[String] = Set()): Set[String] =
    if visited.contains(start) 
      then visited
      else
        adj(start).foldLeft(visited + start)((v, n) => find(n._1, v))
  
  part1      
  part2

  def part1 =

    given Part = PartA

    val result = adj.keys
      .filter(_ != "shiny gold")
      .map(key => find(key).toList).filter(_.contains("shiny gold")).size

    printResult(result)

  def part2 =

    given Part = PartB  

    val a = find("shiny gold").map(name => name -> adj(name)).toMap

    def summa(start: String): Int = {
      if a(start).isEmpty 
        then 0
        else a(start).map { case (k,v) => v.toInt + v.toInt * summa(k) }.sum
    } 
    
    val result = summa("shiny gold")

    printResult(result)