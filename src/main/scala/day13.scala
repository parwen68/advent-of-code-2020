package advent_of_code

import scala.util._

@main def day13 =
  
  given Day = Day(13)
  
  val data = loadData

  part1
  part2

  def part1 = 
    given Part = PartA

    val t = data.split("\n") 
    val d = t(0).toInt
    val n = t(1).split(',').map(v => Try(v.toLong)).collect { case Success(v) => v}

    val r = n
      .map(v => (v, d.toDouble / v.toDouble))
      .map((n, v) => (n, (v.toLong * n) + n))
      .map((n, v) => (n, v, v - d))
      .minBy(_._3)
    
    val result = r._1 * r._3  

    printResult(result)

  def part2 =
    given Part = PartB

    val n = data.split('\n')(1)
      .split(',')
      .zipWithIndex
      .map((v, i) => (Try(v.toLong), i.toLong))
      .collect { case (Success(v), i) => (v, i) }

    def infiniteBigIntList(from: BigInt): LazyList[BigInt] =
      LazyList.cons(from, infiniteBigIntList(from + 1))

    def find(a: (Long,Long), b: (Long,Long)): BigInt =
      infiniteBigIntList(0)
        .map(v => (v, (v + a._2) % a._1, (v + b._2) % b._1))
        .filter{ case (v, a, b) => a == 0 && b == 0 }
        .map(_._1)
        .head

    def comb(n: List[(Long,Long)]): List[((Long, Long),(Long, Long))] =
      n.toList match {
        case h :: tail => tail.map(e => (h, e)) ++ comb(tail)
        case _ => Nil
    }

    val a = comb(n.toList)
      .map{ case (a,b) => (find(a,b), BigInt(a._1 * b._1)) }
      .sortBy(v => v._1 + v._2)
      .reverse
      .toVector
    
    val x = a.head
    val y = a.drop(1)

    val (r, _) = y.foldLeft(x){case (xx, yy) => 
      val s = LazyList.iterate(xx._1)(_ + xx._2)
        .filter { v => (v - yy._1) % yy._2 == 0 }
        .take(2).toList

      (s(0), s(1) - s(0))
    }

    printResult(r)