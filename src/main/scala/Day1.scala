import scala.io.Source

object Day1:
  def main(args: Array[String]): Unit =
    val lines = Util.getLines(Source.fromResource("day1.txt"))
    val (left, right) = lines.map(_.split("\\s+").map(_.toInt) match { case Array(a, b) => (a, b) }).unzip

    val distances = left.sorted.zip(right.sorted).map((a, b) => math.abs(a - b)).sum
    println(s"Day 1 part 1 - $distances")

    val rightCounts = right.groupBy(identity).view.mapValues(_.size)
    val similarity = left.map(n => n * rightCounts.getOrElse(n, 0)).sum
    println(s"Day 1 part 2 - $similarity")
