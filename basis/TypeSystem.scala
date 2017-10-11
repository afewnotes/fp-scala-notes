
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
    // def median(xs: Vector[Number]): Number =???
    
    // 3. 使用适配器模式，解决了扩展性，但必须创建一大堆适配器实例
    trait NumberLike[A] {
        def get: A
        def plus(y: NumberLike[A]): NumberLike[A]
    }
    case class NumberLikeDouble(x: Double) extends NumberLike[Double] {
        def get: Double = x
        def plus(y: NumberLike[Double]) = NumberLikeDouble(x + y.get)
    }
    type Quartile[A] = (NumberLike[A], NumberLike[A], NumberLike[A])
    def median[A](xs: Vector[NumberLike[A]]): NumberLike[A] = xs(xs.size / 2)
    def quartiles[A](xs: Vector[NumberLike[A]]): Quartile[A] =
        (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))
    // ....
    
    // 类型类 
    object Math {
        // 定义特质
        import annnotation.implicitNotFound // 自定义无隐式值可用时的错误消息
        @implicitNotFound("No member of type class NumberLike in scope for ${T}")
        trait NumberLike[T] {
            def plus(x: T, y: T): T
            def divide(x: T, y: Int): T
            def minus(x: T, y: T): T
        }
        
        // 提供默认成员
        object NumberLike {
            implicit object NumberLikeDouble extends NumberLike[Double] {
                def plus(x: Double, y: Double): Double = x + y
                def divide(x: Double, y: Int): Double = x / y
                def minus(x: Double, y: Double): Double = x - y
            }
            implicit object NumberLikeInt extends NumberLike[Int] {
                def plus(x: Int, y: Int): Int = x + y
                def divide(x: Int, y: Int): Int = x / y
                def minus(x: Int, y: Int): Int = x - y
            }
        }
        
    }
    // 运用类型类
    import Math.NumberLike
    def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
        ev.divide(xs.reduce(ev.plus(_, _)), xs.size)
    
    val numbers = Vector[Double](1,2,3,4,5,6) // 隐式参数
    println(mean(numbers))
    
    // 上下文绑定
    // 对于 只有一个 类型参数的隐式参数，scala 提供了 context bound 的简写
    // T: NumberLike 表示必须有一个类型为 NumberLike[T] 的隐式值在当前上下文中可用，等价于隐式参数列表
    def median[T: NumberLike](xs: Vector[T]): T = xs(xs.size / 2)
    def quartiles[T: NumberLike](xs: Vector[T]): (T, T, T) = 
        (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))
    def iqr[T: NumberLike](xs: Vector[T]): T = quartiles(xs) match {
        case (lowerQuartile, _, upperQuartile) =>
            // 调用隐式值
            implicitly[NumberLike[T]].minus(upperQuartile, lowerQuartile)
    }
    
    // 使用场景
    // Numeric
    // Ordering 隐式排序 (集合的 sort 方法)
    // 对象序列化和反序列化
}