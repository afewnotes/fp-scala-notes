object hof {
    
    def factorial(n: Int): Int = {
        // @annotation.tailre
        def go(n: Int, acc: Int): Int = {
            if (n <= 0) acc
            else go(n-1, n*acc)
        }
        
        go(n, 1)
    }
    
    def fib(n: Int): Int = {
        def loop(n: Int, pre: Int, cur: Int): Int = {
            if (n < 2) pre
            else loop(n - 1, cur, pre + cur)
        }
        
        loop(n, 0, 1)
    }
    
    def main(args: Array[String]) {
        print(fib(5))
    }
}