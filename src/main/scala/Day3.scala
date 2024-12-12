import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day3:
  private val multRegex: Regex = """mul\((\d+),(\d+)\)""".r
  private val doString: String = "do()"
  private val dontString: String = "don't()"

  def main(args: Array[String]): Unit =
    val line = Util.getLines(Source.fromResource("day3.txt")).mkString
    val mults = getMults(line)

    val totalMults = foldMults(mults)
    println(s"Day 3 part 1 - $totalMults")

    val totalStrictMults = processCommandString(line)
    println(s"Day 3 part 2 - $totalStrictMults")

  private def getMults(s: String): List[String] = multRegex.findAllIn(s).toList

  private def foldMults(mults: List[String]): Int =
    mults.map { mult =>
      multRegex.findFirstMatchIn(mult).map { matched =>
        val List(a, b) = matched.subgroups.map(_.toInt)
        a * b
      }.get
    }.sum

  private def processCommandString(commandString: String): Int =
    @tailrec def loop(s: String, acc: Int, process: Boolean): Int =
      if s.isEmpty then acc
      else
        if !process && !s.startsWith(doString) then loop(s.tail, acc, process)
        else if !process then loop(s.drop(doString.length), acc, true)
        else if s.startsWith(dontString) then loop(s.drop(dontString.length), acc, false)
        else
          multRegex.findPrefixMatchOf(s) match {
            case Some(matched) =>
              val List(a, b) = matched.subgroups.map(_.toInt)
              loop(s.drop(matched.end), acc + a * b, process)
            case None => loop(s.tail, acc, process)
          }

    loop(commandString, 0, true)
