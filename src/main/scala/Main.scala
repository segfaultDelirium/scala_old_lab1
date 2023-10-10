
import scala.util.{Failure, Success, Try}
object Main {
    def main(args: Array[String]): Unit = {

//        val x = args(0)
        val n = Try(args(0)) match {
            case Success(n) => n.toInt
            case Failure(_exception) => 11
        }

        val notSquares = generateNotSquares(n)
        println(notSquares)

        val notSquares2 = generateNotSquares2(n)
        println(notSquares2)
    }

    def generateNotSquares(n: Int): List[Int] = {
        addToNotSquaresList((1 to n).toList, Set(), List(), n)
    }

    def addToNotSquaresList(list: List[Int], excludeSet: Set[Int], notSquares: List[Int], n: Int): List[Int] = {
        if (list.isEmpty) return notSquares
        val hd :: tail = list
        val square = hd * hd
        val newExcludeSet = if (square < n) excludeSet + square else excludeSet

        // use excludeSet if you want to have '1' in the notSquaresList
        val newNotSquares = if (newExcludeSet.contains(hd)) notSquares else hd :: notSquares
        addToNotSquaresList(tail, newExcludeSet, newNotSquares, n)
    }


    def generateNotSquares2(n: Int): List[Int] = {
        (1 to n).filter(x => {
            val root = math.sqrt(x)
            val isRootInteger = (root - root.toInt).abs < 0.00005
            !isRootInteger
        }).toList
    }

}