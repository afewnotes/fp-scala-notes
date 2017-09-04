object sample {
    
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
    
    def main(args: Array[String]){
        println(failingFn(10))
    }
    
}