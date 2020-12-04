package advent_of_code

@main def day4 =
  
  given Day = Day(4)
  
  val data = loadData
  val valid = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def parseToOneLines(data: String): Array[String] =
    data.split("\n\n")
      .map(_.replace('\n', ' '))

  def lineToMap(line: String) =
    line.split(' ').map(_.splitToTuple(':')).toMap
  
  part1
  part2

  def part1 = 
    given Part = PartA

    val result = parseToOneLines(data)
      .map(lineToMap)
      .filter(s => valid.subsetOf(s.keySet))
      .size

    printResult(result)

  def part2 = 
    given Part = PartB
    
    import scala.util._ 
    val HgtPattern = """(\d+)(cm|in)""".r
    val HclPattern = """#[0-9a-f]{6}""".r
    val PidPattern = """[0-9]{9}""".r
    val validations = Map (
      "byr" -> ((v: String) => Try(v.toInt).map(v => v >= 1920 && v <= 2002).getOrElse(false)),
      "iyr" -> ((v: String) => Try(v.toInt).map(v => v >= 2010 && v <= 2020).getOrElse(false)),
      "eyr" -> ((v: String) => Try(v.toInt).map(v => v >= 2020 && v <= 2030).getOrElse(false)),
      "hgt" -> ((v: String) => Try(v match 
        case HgtPattern(n, t) => 
          if t == "cm" then n.toInt >=150 && n.toInt <= 193
          else if t == "in" then n.toInt >= 59 && n.toInt <= 76
          else false 
      ).getOrElse(false)),
      "hcl" -> ((v: String) => Try( v match { case HclPattern() => true }).getOrElse(false)),
      "ecl" -> ((v: String) => List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(v)),
      "pid" -> ((v: String) => Try( v match { case PidPattern() => true }).getOrElse(false))
    )

    def check(data: String) =
      parseToOneLines(data)
        .map(lineToMap)
        .filter(s => valid.subsetOf(s.keySet))
        .map(_.-("cid"))
        .map(pp => {
          for
            (key, value) <- pp
            valid = validations(key)(value)
          yield valid
        })
        .filter(pp => pp.forall(identity))
        .size


    printResult(check(data))  