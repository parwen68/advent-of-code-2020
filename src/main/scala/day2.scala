package advent_of_code

@main def day2 = 
  given Day = Day(2)
  
  val data = loadData
  val Pattern = """(\d+)-(\d+) (\w): (\w+)""".r
  
  part1
  part2
  
  case class Password(a: Int, b: Int, c: Char, phrase: String)
  
  def parseDataToPassword = data.split("\n").collect { 
    case Pattern(a,b,c,d) => Password(a.toInt, b.toInt, c(0), d) 
  }

  def part1 =
    given Part = PartA

    def checkPassword(password: Password) =
      val Password(min, max, c, phrase) = password  
      val count = phrase.count(_ == c)
      count >= min && count <= max

    printResult(parseDataToPassword.count(checkPassword))

  def part2 = 
    given Part = PartB

    def checkPassword(password: Password) =
      val Password(idx1, idx2, c, phrase) = password
      Seq(idx1, idx2).count(v => phrase(v - 1) == c) == 1

    printResult(parseDataToPassword.count(checkPassword))
        
      