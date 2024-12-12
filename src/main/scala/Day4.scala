import scala.annotation.tailrec
import scala.io.Source

object Day4:
  def main(args: Array[String]): Unit =
    val lines = Util.getLines(Source.fromResource("day4.txt"))
    val matrix = buildMatrix(lines)

    val totalXmasWord = totalValidWord(matrix, "XMAS")
    println(s"Day 4 part 1 - $totalXmasWord")

    val totalCrossMasWord = totalCrossMas(matrix)
    println(s"Day 4 part 2 - $totalCrossMasWord")

  private def buildMatrix(lines: List[String]): Vector[Vector[Char]] = lines.map(_.toVector).toVector

  private def totalCrossMas(matrix: Vector[Vector[Char]]): Int =
    (for {
      y <- matrix.indices
      x <- matrix(0).indices
    } yield isCrossMas(x, y, matrix)).count(identity)

  final case class Check(direction: (Int, Int), point: (Int, Int))
  private def isCrossMas(x: Int, y: Int, matrix: Vector[Vector[Char]]): Boolean =
    // check that the point can form the word "MAS" diagonally in both directions with the "A" being the middle point
    // something like this but in all directions
    // M.S
    // .A.
    // M.S
    if matrix(y)(x) != 'A' then false
    else
      val word = "MAS"
      val checks =
        List(
          Check((1, 1), (x - 1, y - 1)),
          Check((-1, -1), (x + 1, y + 1)),
          Check((1, -1), (x - 1, y + 1)),
          Check((-1, 1), (x + 1, y - 1))
        )

      checks.count { case Check((dx, dy), (px, py)) =>
        validWord(px, py, matrix, word, dx, dy)
      } == 2

  private def totalValidWord(matrix: Vector[Vector[Char]], word: String): Int =
    (for {
      y <- matrix.indices
      x <- matrix(0).indices
    } yield countValidWord(x, y, matrix, word)).sum

  private def countValidWord(x: Int, y: Int, matrix: Vector[Vector[Char]], word: String): Int =
    val directions =
      (for {
        dx <- -1 to 1
        dy <- -1 to 1
        if dx != 0 || dy != 0
      } yield dx -> dy).toList

    directions.count((dx, dy) => validWord(x, y, matrix, word, dx, dy))

  private def validWord(x: Int, y: Int, matrix: Vector[Vector[Char]], word: String, dx: Int, dy: Int): Boolean =
    @tailrec def loop(x: Int, y: Int, word: String, acc: Boolean): Boolean =
      if word.isEmpty then true
      else if x < 0 || y < 0 || y >= matrix.length || x >= matrix(0).length then false
      else if matrix(y)(x) != word.head then false
      else loop(x + dx, y + dy, word.tail, true)

    loop(x, y, word, false)
