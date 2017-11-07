object Sample {
    
    // 逐一叠加
    def sum(ints: Seq[Int]): Int = 
        ints.foldLeft(0)((a,b) => a+b)
        
    // 分治
    def sum(ints: IndexedSeq[Int]): Int = // IndexedSeq 标准库中随机访问序列的超类
        if (ints.size <= 1)
            ints.headOption getOrElse 0
        else {
            val (l, r) = ints.splitAt(ints.length / 2) // splitAt 方法基于特定的 index 将自身一分为二
            sum(l) + sum(r) // 递归相加
        }
        
    def sum(ints: IndexedSeq[Int]): Int = 
        if (ints.size <= 1)
            ints headOption getOrElse 0
        else {
            val (l, r) => ints.splitAt(ints.length / 2)
            val sumL: Par[Int] = Par.unit(sum(l)) // 并行计算左半部分
            val sumR: Par[Int] = Par.unit(sum(r)) // 并行计算右半部分
            Par.get(sumL) + Par.get(sumR) // 抽取结果求和
        }
}
    
object Par[A] {
    // 并行化单元
    // 接收一个未求值的A，返回结果会在另一个独立的 线程中完成求值
    def unit[A](a: => A): Par[A]
    
    // 从并行计算里抽取结果
    def get[A](a: Par[A]): A 
}