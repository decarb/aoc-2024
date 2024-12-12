import scala.io.Source
import scala.util.Using

object Util:
  def getLines(source: Source): List[String] = Using(source)(_.getLines().toList).fold(throw _, identity)