object Polymorphism {
    
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
        // (a: A) => (b => f(a, b))
        a => b => f(a, b)
    }
 
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }
}