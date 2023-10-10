import scala.util.Random

object Exercise2WithPointClass extends App {

    case class Point(x: Double, y: Double) {

    }

    def createRandomPoint(random: Random): Point = {
        Point(random.nextDouble(), random.nextDouble())
    }

    def distanceTo0(point: Point): Double = {
        math.sqrt(math.pow(point.x, 2) + math.pow(point.y, 2))
    }

    val n = 10000
    val random = new Random()
    val innerPointsCount = (1 to n).map(_i => createRandomPoint(random)).filter(p => distanceTo0(p) < 1).length

    val pi = 4.0 * (innerPointsCount.toDouble / n.toDouble)
    println(pi)


}
