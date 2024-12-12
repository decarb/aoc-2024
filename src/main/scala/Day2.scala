import scala.annotation.tailrec
import scala.io.Source

object Day2:
  def main(args: Array[String]): Unit =
    val lines = Util.getLines(Source.fromResource("day2.txt"))
    val levels = lines.map(_.split("\\s").map(_.toInt).toList)

    val totalSafe = levels.map(isSafe).count(identity)
    println(s"Day 2 part 1 - $totalSafe")

    val replacedTotalSafe = levels.map(combinations).count(_.exists(isSafe))
    println(s"Day 2 part 2 - $replacedTotalSafe")

  private def isSafe(report: List[Int]): Boolean =
    @tailrec def loop(lastLevel: Int, lastDiff: Int, tail: List[Int], acc: Boolean): Boolean =
      if (tail.isEmpty) acc
      else
        val currentLevel = tail.head
        val diff = currentLevel - lastLevel
        if (diff * lastDiff > 0 && math.abs(diff) <= 3) loop(currentLevel, diff, tail.tail, true)
        else false

    loop(report.head, report.tail.head - report.head, report.tail, true)

  private def combinations(report: List[Int]): List[List[Int]] =
    @tailrec def loop(n: Int, acc: List[List[Int]]): List[List[Int]] =
      if (n == report.length) acc
      else
        val newList = report.take(n) ++ report.drop(n + 1)
        loop(n + 1, acc :+ newList)

    report +: loop(0, List.empty)
