import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day5:
  def main(args: Array[String]): Unit =
    val lines = Util.getLines(Source.fromResource("day5.txt"))
    val (rules, checks) = lines.span(_.nonEmpty) match {
      case (before, _ :: after) => (before.map(Rule.fromString), after.map(_.split(",").map(_.toInt).toList))
      case (before, Nil) => (before.map(Rule.fromString), Nil)
    }

    val newTotal = checks.filter(checkCheck(_, rules).isRight).map(c => c(c.length / 2)).sum
    println(s"Day 5 part 1 - $newTotal")

    val bad = checks.filter(checkCheck(_, rules).isLeft)
    val middles = fixChecks(bad, rules).map(c => c(c.length / 2)).sum
    println(s"Day 5 part 2 - $middles")

  final case class Rule(before: Int, after: Int)
  private object Rule:
    private val ruleRegex: Regex = """(\d+)\|(\d+)""".r
    def fromString(s: String): Rule = s match {
      case ruleRegex(before, after) => Rule(before.toInt, after.toInt)
    }

  private def checkCheck(check: List[Int], rules: List[Rule]): Either[List[Rule], List[Int]] =
    val applicableRules = rules.filter(r => check.contains(r.before) && check.contains(r.after))
    val (ruleBreakers, isGood) = applicableRules.foldLeft((List.empty[Rule], true)) { case ((nos, acc), r) =>
      val beforeIndex = check.indexOf(r.before)
      val afterIndex = check.indexOf(r.after)
      if beforeIndex < afterIndex then (nos, acc)
      else (nos :+ r, false)
    }

    Either.cond(isGood, check, ruleBreakers)

  private def swapCheck(check: List[Int], rule: Rule): List[Int] =
    val beforeIndex = check.indexOf(rule.before)
    val afterIndex = check.indexOf(rule.after)
    check.updated(beforeIndex, rule.after).updated(afterIndex, rule.before)

  @tailrec
  private def fixCheck(check: List[Int], rules: List[Rule], brickedRules: List[Rule]): List[Int] =
    if (brickedRules.isEmpty) check
    else
      val bricked = brickedRules.head
      val newCheck = swapCheck(check, bricked)
      checkCheck(newCheck, rules) match
        case Right(check) => check
        case Left(bricked) => fixCheck(newCheck, rules, bricked)

  private def fixChecks(checks: List[List[Int]], rules: List[Rule]): List[List[Int]] =
    checks.map { check =>
      val checking = checkCheck(check, rules)
      checking match {
        case Right(_) => check
        case Left(bricked) => fixCheck(check, rules, bricked)
      }
    }
