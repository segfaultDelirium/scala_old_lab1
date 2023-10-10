import scala.util.Random

object Exercise2 extends App{

    println("calculate value of Pi by monte carlo on circle")

    val random = new Random()
    val n = 100000
    val points = (1 to n).map(_x => generateRandomPair(random)).toList

    val (inner, outer) = splitPointsToInnerAndOuter(points)
//    println(s"inner.length: ${inner.length}, outer.length: ${outer.length}")
    val pi = 4 * inner.length.toDouble / (inner.length.toDouble + outer.length.toDouble)
    println(pi)

    val (innerCount, outerCount) = startCountInnerAndOuter(points)
//    println(s"innerCount: ${innerCount}, outerCount: ${outerCount}")
    val pi2 = 4 * (innerCount.toDouble / (innerCount.toDouble + outerCount.toDouble))
    println(pi2)

    val pi3 = 4 * (points.filter(p => calculateDistanceTo0_0(p) < 1 ).length.toDouble / n.toDouble)
    println(pi3)



    def generateRandomPair(random: Random): (Double,Double) = {
        (random.nextDouble(), random.nextDouble())
    }


    def splitPointsToInnerAndOuter(points: List[(Double, Double)]):(List[(Double, Double)], List[(Double, Double)]) = {
        addPointsToInnerOrOuterRec(List(), List(), points)
    }

    def addPointsToInnerOrOuterRec(inner: List[(Double, Double)], outer: List[(Double, Double)], points: List[(Double, Double)])
    : (List[(Double, Double)], List[(Double, Double)]) = {
        if(points.isEmpty) return (inner, outer)
        val point :: tail = points
        val r = math.sqrt( math.pow(point._1, 2) + math.pow(point._2, 2) )
        val (newInner, newOuter) = if(r < 1) (point :: inner, outer) else (inner, point :: outer)
        addPointsToInnerOrOuterRec(newInner, newOuter, tail)
    }


    def startCountInnerAndOuter(points: List[(Double, Double)]): (Int, Int) = countInnerAndOuter(0,0, points)

    def countInnerAndOuter(innerCount: Int, outerCount: Int, points: List[(Double, Double)]): (Int, Int) = {
        if(points.isEmpty) return (innerCount, outerCount)
        val point :: tail = points
        val r = math.sqrt(math.pow(point._1, 2) + math.pow(point._2, 2))
        val (newInnerCount, newOuterCount) = if (r < 1) (innerCount + 1, outerCount) else (innerCount, outerCount + 1)
        countInnerAndOuter(newInnerCount, newOuterCount, tail)
    }

    def calculateDistanceTo0_0(point: (Double, Double)): Double = math.sqrt( math.pow(point._1, 2) + math.pow(point._2, 2))

}
