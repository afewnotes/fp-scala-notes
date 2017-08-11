object hof {
    
    def factorial(n: Int): Int = {
        // @annotation.tailre
        def go(n: Int, acc: Int): Int = {
            if (n <= 0) acc
            else go(n-1, n*acc)
        }
        
        go(n, 1)
    }
    
    // http://cjwebb.github.io/blog/2013/10/30/fibonacci-numbers-in-scala/
    
    def fib(n: Int): Int = {
        def loop(n: Int, pre: Int, cur: Int): Int = {
            
            // if (n < 2) pre
            // else loop(n - 1, cur, pre + cur)
            
            n match {
                case 0 => pre
                case _ => loop(n - 1, cur, pre + cur)
            }
        }
        
        loop(n, 0, 1)
    }
    
    // hof
    def formatResult(name: String, n: Int, f: Int => Int): String = {
        "the %s of %d is %d".format(name, n, f(n))
    }
 
    // def findFirst(ss: Array[String], key: String): Int = {
    //     def loop(n: Int): Int = 
    //         if (n >= ss.length) -1
    //         else if (ss(n) == key) n
    //         else loop(n + 1)
            
    //     loop(0)
    // }
        
    // 泛型函数
    def findFirst[A](ss: Array[A], p: A => Boolean): Int = {
        def loop(n: Int): Int = 
            if (n >= ss.length) -1
            else if (p(ss(n))) n
            else loop(n + 1)
            
        loop(0)
    }
    
    // p2.2
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        def loop(n: Int): Boolean = 
            if (n >= as.length - 1) true
            else if (ordered(as(n), as(n + 1))) loop(n+1)
            else false
            
        loop(0)
    }
    
    
    def main(args: Array[String]) {
        println(formatResult("factorial", 5, factorial))
        println(formatResult("fibonacci", 5, fib))
        
        println(isSorted(Array(1, 9, 5, 7), 
                    // 匿名函数、字面量函数
                    (x: Int, y: Int) => x < y))
        println(isSorted(Array(1, 3, 5, 7), (x: Int, y: Int) => x < y))
    }
}