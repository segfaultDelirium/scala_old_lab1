object ComplexClass extends App {

    case class Complex(re: Double, im: Double){
        def +(complex: Complex): Complex = {
            Complex(re + complex.re, im + complex.im)
        }

        def -(complex: Complex): Complex = {
            Complex(re - complex.re, im - complex.im)
        }

        override def toString(): String = {
            s"Re:{$re} Im:{$im}"
        }

        def r: Double = math.sqrt(math.pow(re, 2) + math.pow(im, 2))

        def angle: Double = {
            math.asin(im / r)
        }
    }

    //    println(math.cos(math.Pi / 6.0))

    object Complex {
        def polar(radius: Double, angle: Double): Complex = {
            val x = radius * math.cos(angle)
            val y = radius * math.sin(angle)
            Complex(x, y)
        }

    }

    val x = Complex(1, 2)
    val y = Complex(3, 2)

    println(s"x: $x, y: $y, x + y: ${x + y}, x - y: ${x - y}")

    val f = Complex.polar(radius = 3, angle = math.Pi)
    println(f)

    println( "|x| "+ x.r + " angle " +x.angle)


}
