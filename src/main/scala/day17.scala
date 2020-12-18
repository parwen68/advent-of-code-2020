package advent_of_code

import scala.util._

@main def day17 =
  
  given Day = Day(17)

  val data = loadData

  case class Coord(x: Int, y: Int, z: Int, w: Int = 0) {
    def +(c: Coord) = Coord(x + c.x, y + c.y, z + c.z, w + c.w)
  }

  val init = data.split("\n").zipWithIndex.foldLeft(Set[Coord]()){
    case (world, (line, y)) => line.zipWithIndex.flatMap { (e, x) => 
      e match 
        case '#' => Some(Coord(x, y, 0))
        case _ => None
    }.toSet ++ world
  }

  def maxAndMin(seq: Set[Int]) = (seq.max, seq.min)

  part1
  part2

  def part1 =

    given Part = PartA

    val neigh =
      for 
        x <- -1 to 1 
        y <- -1 to 1
        z <- -1 to 1
        if !(x == 0 && y == 0 && z == 0)
      yield Coord(x, y, z)

    def getNeighs(c: Coord, w: Set[Coord]) = 
      neigh.map(n => c + n).map(w.contains).count(identity)

    def cycle(world: Set[Coord]): LazyList[Set[Coord]] =
      val (xMax, xMin) = maxAndMin(world.map(_.x))
      val (yMax, yMin) = maxAndMin(world.map(_.y))
      val (zMax, zMin) = maxAndMin(world.map(_.z))

      val area = 
        for 
          x <- xMin - 1 to xMax + 1
          y <- yMin - 1 to yMax + 1
          z <- zMin - 1 to zMax + 1
        yield Coord(x, y, z) 
      
      val nextWorld = area.flatMap(c => {
        val neighs = getNeighs(c, world) 
        if world.contains(c) && ((2 to 3) contains neighs) ||
          !world.contains(c) && neighs == 3 
          then Option(c) 
          else None
      }).toSet

      nextWorld #:: cycle(nextWorld)

    val w = cycle(init).drop(5).head

    printResult(w.size)

  def part2 =

    given Part = PartB

    val neigh =
      for 
        x <- -1 to 1 
        y <- -1 to 1
        z <- -1 to 1
        w <- -1 to 1
        if !(x == 0 && y == 0 && z == 0 && w == 0)
      yield Coord(x, y, z, w)

    def getNeighs(c: Coord, w: Set[Coord]) = 
      neigh.map(n => c + n).map(w.contains).count(identity)

    def cycle(world: Set[Coord]): LazyList[Set[Coord]] =
      val (xMax, xMin) = maxAndMin(world.map(_.x))
      val (yMax, yMin) = maxAndMin(world.map(_.y))
      val (zMax, zMin) = maxAndMin(world.map(_.z))
      val (wMax, wMin) = maxAndMin(world.map(_.w))

      val area = 
        for 
          x <- xMin - 1 to xMax + 1
          y <- yMin - 1 to yMax + 1
          z <- zMin - 1 to zMax + 1
          w <- zMin - 1 to wMax + 1
        yield Coord(x, y, z, w) 
      
      val nextWorld = area.flatMap(c => {
        val neighs = getNeighs(c, world) 
        if world.contains(c) && ((2 to 3) contains neighs) ||
           !world.contains(c) && neighs == 3 
          then Option(c) 
          else None
      }).toSet

      nextWorld #:: cycle(nextWorld)

    val w = cycle(init).drop(5).head

    printResult(w.size)