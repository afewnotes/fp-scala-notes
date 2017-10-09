
object TypeSystem {
    
    object Statistics {
        def median(xs: Vector[Double]): Double = xs(xs.size / 2)
        def quartiles(xs: Vector[Double]): (Double, Double, Double) =
            (xs(xs.size/4), median(xs), xs(xs(xs.size / 4 * 3))
        def iqr(xs: Vector[Double]): Double = quartiles(xs) match {
            case (lowerQuartile, _, upperQuartile) => upperQuartile - lowerQuartile
        }
        def mean(xs: Vector[Double]): Double = {
            xs.reduce(_ + _) / xs.size
        }
    }
    
    
}