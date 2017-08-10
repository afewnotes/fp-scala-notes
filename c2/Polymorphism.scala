object Polymorphism {
    
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
        // (a: A) => (b => f(a, b))
        // => 右箭头为 右结合 
        a => b => f(a, b)
    }
 
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }
    
    def compose[A,B,C](f: B => C, g: A => B): A => C = {
        a => f(g(a))
    }
    
    // Function1 的 compose 或 andThen
    // val f = (x: Double) => math.Pi / 2-x
    // val cos = f andThen math.sin
}