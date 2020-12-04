package advent_of_code

opaque type Day = Int
object Day:
  def apply(day: Int): Day = day
  extension (day: Day) def fileName = s"$day.txt"

opaque type Part = Char
object Part:
  def apply(c: Char): Part = c
val PartA = Part('a')
val PartB = Part('b')

def printResult[T](result: T)(using day: Day, part: Part) = println(s"DAY$day($part), Result: ${result}")

def infiniteList(from: Long = 0): LazyList[Long] =
  LazyList.cons(from, infiniteList(from + 1))

def lazyList(from: Long, to: Long): LazyList[Long] =
  if from >= to 
    then LazyList.empty
    else LazyList.cons(from, lazyList(from + 1, to))

extension (s: String) def splitToTuple(c: Char) =
  val splitted = s.split(c)
  (splitted(0), splitted(1))

def loadData(using day: Day): String = 
  given Day = day 
    fetching.loadData

object fetching:
  import java._, net._, io._, nio.charset._
  import scala.annotation.tailrec

  val cacheDir = File(System.getProperty("user.home"), ".aoc-cache")
  val cookieFile = File(System.getProperty("user.home"), ".aoc-cookie")

  def loadData(using day: Day): String =
    createCacheDirIfNotExists
    val cacheFile = File(cacheDir, day.fileName)
    if cacheFile.exists 
      then readContent(new FileInputStream(cacheFile))
      else fetchAndSave(cacheFile, day)

  private def createCacheDirIfNotExists =
    if !cacheDir.exists then cacheDir.mkdir

  private def fetchAndSave(cacheFile: File, day: Day): String =
    val content = fetch(day)
    saveContent(cacheFile, content)

  private def fetch(day: Day) = 
    val urlConnection = URL(s"https://adventofcode.com/2020/day/$day/input").openConnection()
    urlConnection.setRequestProperty("Cookie", s"session=${loadCookie.trim}")
    readContent(urlConnection.getInputStream)

  private def loadCookie =
    readContent(FileInputStream(cookieFile))
    
  private def saveContent(f: File, content: String) =
    val bw = BufferedWriter(FileWriter(f))
    try
      bw.write(content)
    finally  
      bw.close()
    
    content
  
  private def readContent(is: InputStream) =
    @tailrec def readChars(br: BufferedReader, acc: String = ""): String =
      val c = br.read
      if (c != -1)
        readChars(br, acc + c.toChar)
      else
        acc
    
    val br = BufferedReader(InputStreamReader(is, StandardCharsets.UTF_8))    
    try    
      readChars(br)
    finally
      br.close();

object FetchData:
  def main(args: Array[String]): Unit = 
    given Day = Day(args(0).toInt)
    println(loadData)

