object sample {
    
    def main(args: Array[String]){
        println(failingFn(10))
    }
    
    def failingFn(i: Int): Int = {
        // 不是引用透明 （表达式可以被它引用的值替代，不依赖上下文，可本地推导）
        // val y: Int = throw new Exception("fail") 
        try {
            val x = 42 + 5
            // x + y                                 // 抛出异常
            x + ((throw new Exception("fail")): Int) // 返回 43
        }
        catch {
            case e: Exception => 43
        }
    }
    
    // 部分函数：对一些输入没有结果
    // 缺点：
    //  - 允许错误无声的传播，调用者可能忘了检查，导致后续代码不能正常工作;
    //  - 显式 if 检测数据正确性
    //  - 不适用于多态
    //  - 需要调用者做一些事情
    def mean(xs: Seq[Double]): Double = 
        if (xs.isEmpty)
            throw new ArithmeticException("mean of empty list")
        else xs.sum / xs.length
        
    // 完全函数
    // 缺点：需要调用者知道如何处理未定义的情况
    def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double = 
        if (xs.isEmpty) onEmpty
        else xs.sum / xs.length
        
    // Option：结果不一定总被定义
    sealed trait Option[+A]
    // 对应已被定义的情况
    case class Some[+A](get: A) extends Option[A]
    // 对应未被定义的情况
    case object None extends Option[Nothing]
    
    // 完全函数：每个输入值都有对应输出类型的值
    def mean_2(xs: Seq[Double]): Option[Double] = 
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)
}