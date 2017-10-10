
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
    // 思考：如果想支持更多类型，如 Int ，如何处理？
    
    // 1. 重载方法，代码会冗余
    // 2. 扩展共同的基类 Number，丢掉了先前的类型信息，不易扩展：不能强制其他类型扩展 Number 特质
    // 3. 使用适配器模式，解决了扩展性，但必须创建一大堆适配器实例
    
    // 类型类
}