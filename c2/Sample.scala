object Sample {  // 单例对象（同时声明一个类和唯一实例）

    // 接收 Int, 返回 Int
    def abs(n: Int): Int = {
        if (n < 0) -n
        else n
    }
    
    private def formatAbs(x: Int) = {
        val msg = "absolute value of %d is %d"
        msg.format(x, abs(x))
    }
    
    def main(args: Array[String]): Unit = {
        println(formatAbs(-42))
    }
}